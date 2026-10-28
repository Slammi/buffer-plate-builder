# Constraint diagnostics.
#
# Feasibility rule: a well fits only if  sum(C_i / Stock_i) + fixed / V_total <= 1.
# V_total cancels out, so changing the final volume never helps; only stocks,
# the fixed volume or the design ranges do.

#' Headline counts and the worst-case well.
feasibility_summary <- function(runs, volumes, settings) {
  worst <- which.max(runs$fraction_used)
  data.frame(
    runs = nrow(runs),
    wells = nrow(volumes),
    plates = max(volumes$plate),
    ok = sum(volumes$status == "OK"),
    over_volume = sum(volumes$status == "OVER VOLUME"),
    below_min = sum(volumes$status == "BELOW MIN"),
    worst_fraction = runs$fraction_used[worst],
    worst_run = runs$run[worst],
    headroom_fraction = 1 - runs$fraction_used[worst],
    headroom_uL = (1 - runs$fraction_used[worst]) * settings$final_volume,
    stringsAsFactors = FALSE
  )
}

#' Over-volume runs: how far over, and which component takes the largest share.
failing_wells <- function(runs, volumes) {
  bad <- runs[runs$over_volume, , drop = FALSE]
  if (!nrow(bad)) {
    return(data.frame(run = integer(), wells = character(), fraction_used = numeric(),
                      overflow_uL = numeric(), largest_component = character(),
                      largest_volume = numeric(), largest_share = numeric()))
  }
  wells <- vapply(bad$run, function(r) {
    v <- volumes[volumes$run == r, ]
    paste(paste0(if (max(volumes$plate) > 1) paste0("P", v$plate, ":") else "", v$well),
          collapse = ", ")
  }, character(1))
  out <- data.frame(
    run = bad$run,
    wells = wells,
    fraction_used = bad$fraction_used,
    overflow_uL = -bad$water,
    largest_component = bad$largest_component,
    largest_volume = bad$largest_volume,
    largest_share = bad$largest_volume / (bad$component_total + bad$fixed),
    stringsAsFactors = FALSE
  )
  out[order(-out$overflow_uL), ]
}

#' Minimum stock per component for every well to fit, holding the other
#' stocks (as actually used, incl. working stocks) and the fixed volume fixed.
#' The component itself is assumed to come from its main stock.
min_stocks <- function(cv, stocks, settings) {
  V <- settings$final_volume
  frac <- cv$volume / V
  run_frac <- tapply(frac, cv$run, sum) + settings$fixed_volume / V
  rows <- lapply(seq_len(nrow(stocks)), function(i) {
    s <- stocks[i, ]
    x <- cv[cv$component == s$component, ]
    own <- tapply(x$volume / V, x$run, sum)
    avail <- 1 - (run_frac[names(own)] - own)
    conc <- tapply(x$conc, x$run, max)
    used <- conc > 0
    need <- if (!any(used)) NA_real_ else {
      if (any(avail[used] <= VOL_TOL)) Inf else max(conc[used] / avail[used])
    }
    data.frame(
      component = s$component,
      role = s$role,
      units = s$units,
      stock_conc = s$stock_conc,
      min_stock_needed = need,
      wells_blocked_by_others = sum(avail[used] <= VOL_TOL),
      max_stock_conc = s$max_stock_conc,
      fits_now = !is.na(need) && s$stock_conc >= need * (1 - 1e-9),
      exceeds_max = !is.na(need) && !is.na(s$max_stock_conc) && need > s$max_stock_conc,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Which stocks cause failing runs, and what would fix them.
#'
#' One row per source (each stock, working stocks separately, then water):
#' - runs_below_min: runs where this source's volume is above 0 but under the
#'   minimum pipettable volume. Counted in every run, including over-volume
#'   ones (the Inputs summary counts each run once, OVER VOLUME first).
#' - over_volume_runs: over-volume runs this source is dispensed into.
#' - largest_in_over_volume: over-volume runs where it is the largest volume.
#' - problem: a plain-language fix (largest working stock that keeps the
#'   smallest level pipettable; smallest stock that makes every well fit).
#' - status: "ok", "warn" (below min only) or "bad" (largest in over-volume runs).
stock_problems <- function(result) {
  s <- result$settings
  V <- s$final_volume
  min_v <- s$min_volume
  cv <- result$concentrations
  runs <- result$runs
  over_runs <- runs$run[runs$over_volume]
  largest_over <- runs$largest_component[runs$over_volume]
  sources <- unique(cv$source[order(match(cv$component, result$stocks$component))])

  rows <- lapply(sources, function(src) {
    x <- cv[cv$source == src, ]
    comp <- x$component[1]
    st <- result$stocks[result$stocks$component == comp, ]
    working <- grepl(" \\(working\\)$", src)
    stock <- if (working) st$working_stock_conc else st$stock_conc
    used <- x$volume > VOL_TOL
    small <- used & x$volume < min_v - VOL_TOL
    n_largest <- if (working) 0L else sum(largest_over == comp)
    ms <- result$min_stocks[result$min_stocks$component == comp, ]

    fixes <- character()
    if (any(small)) {
      max_work <- min(x$conc[small]) * V / min_v
      fixes <- c(fixes, sprintf(
        "Below min in %d run(s): use a %sworking stock of at most %s %s for the low levels.",
        sum(small), if (working) "more dilute " else "", fmt_conc(signif(max_work, 3)), st$units))
    }
    if (n_largest > 0) {
      need <- ms$min_stock_needed
      fixes <- c(fixes, if (is.finite(need)) {
        sprintf("Largest volume in %d over-volume run(s): a stock of at least %s %s makes every well fit%s.",
                n_largest, fmt_conc(signif_up(need, 3)), st$units,
                if (isTRUE(ms$exceeds_max)) " (above its max stock)" else "")
      } else {
        sprintf("Largest volume in %d over-volume run(s), but raising this stock alone can't make them fit.",
                n_largest)
      })
    }
    data.frame(
      source = src, units = st$units, stock_conc = stock,
      runs_below_min = sum(small),
      smallest_volume_uL = if (any(used)) min(x$volume[used]) else NA_real_,
      over_volume_runs = sum(used & x$run %in% over_runs),
      largest_in_over_volume = n_largest,
      min_stock_to_fit = if (working) NA_real_ else ms$min_stock_needed,
      problem = if (length(fixes)) paste(fixes, collapse = " ") else "OK",
      status = if (n_largest > 0) "bad" else if (any(small)) "warn" else "ok",
      stringsAsFactors = FALSE
    )
  })

  # Water: below min means the well is nearly full before water is added.
  water <- runs$water
  w_small <- water > VOL_TOL & water < min_v - VOL_TOL
  rows[[length(rows) + 1]] <- data.frame(
    source = "Water", units = "", stock_conc = NA_real_,
    runs_below_min = sum(w_small),
    smallest_volume_uL = if (any(water > VOL_TOL)) min(water[water > VOL_TOL]) else NA_real_,
    over_volume_runs = NA_integer_, largest_in_over_volume = NA_integer_, min_stock_to_fit = NA_real_,
    problem = if (any(w_small)) sprintf(paste0(
      "Below min in %d run(s): those wells are almost full before water. Use more concentrated ",
      "stocks for their largest components (or less fixed volume) to leave at least %s µL of water."),
      sum(w_small), fmt_conc(min_v)) else "OK",
    status = if (any(w_small)) "warn" else "ok",
    stringsAsFactors = FALSE
  )

  out <- do.call(rbind, rows)
  out <- out[order(match(out$status, c("bad", "warn", "ok"))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Explain why only infinite stocks would fit. Usually the components whose
# stock cannot change (held, or already at their max) plus the fixed volume
# fill whole wells on their own.
infinite_stock_message <- function(C, stocks, held, infinite, settings) {
  V <- settings$final_volume
  fixed_share <- settings$fixed_volume / V
  at_max <- !is.na(stocks$max_stock_conc) & stocks$stock_conc >= stocks$max_stock_conc
  fixed_stock <- held | at_max
  share <- sweep(C, 2, stocks$stock_conc, "/")
  locked <- rowSums(share[, fixed_stock, drop = FALSE]) + fixed_share
  others <- rowSums(share[, infinite, drop = FALSE]) > 0
  full <- which(locked >= 1 - 1e-9 & others)
  inf_names <- paste(stocks$component[infinite], collapse = ", ")

  if (length(full)) {
    r <- full[which.max(locked[full])]
    j <- which(fixed_stock)[which.max(share[r, fixed_stock])]
    cmp <- stocks$component[j]
    why <- if (held[j]) "held" else "at its max stock"
    advice <- if (held[j]) {
      sprintf("Stop holding %s so its stock can be raised, or lower its maximum level in the design.", cmp)
    } else {
      sprintf("Raise the max stock for %s if a more concentrated stock can be made, or lower its maximum level in the design.", cmp)
    }
    sprintf(paste0(
      "Not feasible: %s is %s at %s, and in %d run(s) it%s uses %.0f%% of the well ",
      "(e.g. run %d: level %s / stock %s), leaving no room for %s. ",
      "Only infinitely concentrated stocks of those would fit. %s"),
      cmp, why, format(stocks$stock_conc[j]), length(full),
      if (sum(fixed_stock) > 1 || fixed_share > 0) " (with the other unchangeable volumes)" else " alone",
      100 * locked[r], r, format(C[r, j]), format(stocks$stock_conc[j]), inf_names, advice)
  } else {
    sprintf(paste0(
      "Not feasible: every well would fit only if %s had infinitely concentrated stocks. ",
      "Set a realistic max stock for them, or narrow the design ranges or reduce the fixed volume."),
      inf_names)
  }
}

# Round up to `digits` significant figures (keeps a feasible stock feasible).
signif_up <- function(x, digits = 2) {
  out <- x
  ok <- is.finite(x) & x > 0
  p <- 10^(floor(log10(x[ok])) - digits + 1)
  out[ok] <- ceiling(x[ok] / p - 1e-6) * p
  out
}

#' Find stocks (only ever more concentrated than now, never above
#' max_stock_conc) that make every well fit, using a linear programme in the
#' inverse stocks x_i = 1/Stock_i:
#'
#'   maximise  sum(Stock_now_i * x_i)          (stay as close to current as possible)
#'   s.t.      sum(C_ri * x_i) <= 1 - fixed/V  for every run r
#'             1/max_i <= x_i <= 1/Stock_now_i
#'
#' Working stocks are ignored (main stocks only). `hold` names components
#' whose stock must not change. A solution that needs an infinitely
#' concentrated stock (x_i = 0, possible when no max is set) is reported as
#' not feasible, with the reason.
optimise_stocks <- function(result, hold = character()) {
  stocks <- result$stocks
  conc <- result$concentrations
  s <- result$settings
  comps <- stocks$component
  C <- vapply(comps, function(cmp) {
    x <- conc[conc$component == cmp, ]
    x$conc[order(x$run)]
  }, numeric(length(unique(conc$run))))
  C <- matrix(C, ncol = length(comps))
  rhs_fit <- 1 - s$fixed_volume / s$final_volume

  n <- length(comps)
  upper <- 1 / stocks$stock_conc
  lower <- ifelse(is.na(stocks$max_stock_conc), 0, 1 / stocks$max_stock_conc)
  held <- tolower(comps) %in% tolower(hold)
  lower[held] <- upper[held]
  lower <- pmin(lower, upper)

  # Solve in z_i = Stock_now_i * x_i (1 = unchanged, 0.5 = twice as
  # concentrated), which keeps the numbers well scaled. boot::simplex() is
  # plain R: the compiled lpSolve crashed the in-browser (Shinylive) build
  # when the optimiser was run a second time.
  # Stocks that cannot change (held, or already at their max) are taken out:
  # their share of each well is known and is subtracted from the room left.
  A <- sweep(C, 2, stocks$stock_conc, "/")
  z_low <- lower * stocks$stock_conc
  fixed <- z_low >= 1 - 1e-12
  room <- rhs_fit - rowSums(A[, fixed, drop = FALSE])
  free <- which(!fixed)
  Af <- A[, free, drop = FALSE]
  keep <- rowSums(Af) > 0
  rows <- unique(cbind(Af, room)[keep, , drop = FALSE])
  Af <- rows[, seq_along(free), drop = FALSE]
  room <- pmax(rows[, length(free) + 1], 0)
  has_low <- z_low[free] > 0

  z <- rep(1, n)
  feasible <- all(rhs_fit - rowSums(A[, fixed, drop = FALSE]) >= -1e-9)
  if (feasible && length(free)) {
    sol <- boot::simplex(
      a = rep(1, length(free)),
      A1 = rbind(Af, diag(length(free))), b1 = c(room, rep(1, length(free))),
      A2 = if (any(has_low)) diag(length(free))[has_low, , drop = FALSE],
      b2 = if (any(has_low)) z_low[free][has_low],
      maxi = TRUE, n.iter = 20 * (length(free) + nrow(Af)))
    feasible <- sol$solved == 1
    z[free] <- sol$soln
  }
  if (!feasible) z <- rep(NA_real_, n)
  x <- z / stocks$stock_conc

  # Without a max stock the LP may drive x_i to 0, i.e. an infinitely
  # concentrated stock that uses no volume. That is not a real solution.
  infinite <- if (feasible) colSums(C) > 0 & z <= 1e-9 else rep(FALSE, n)
  if (any(infinite)) {
    return(list(
      feasible = FALSE,
      message = infinite_stock_message(C, stocks, held, infinite, s),
      worst_fraction = NA_real_,
      infinite_components = comps[infinite],
      stocks = data.frame(
        component = comps, units = stocks$units, stock_conc = stocks$stock_conc,
        max_stock_conc = stocks$max_stock_conc, suggested_stock = NA_real_,
        suggested_rounded = NA_real_, changed = NA, stringsAsFactors = FALSE
      )
    ))
  }

  suggested <- ifelse(abs(z - 1) <= 1e-9, stocks$stock_conc, 1 / x)
  rounded <- ifelse(suggested == stocks$stock_conc, suggested, signif_up(suggested, 2))
  if (feasible) {
    over_max <- !is.na(stocks$max_stock_conc) & rounded > stocks$max_stock_conc
    rounded[over_max] <- suggested[over_max]
  }
  worst <- if (feasible) max(C %*% (1 / rounded)) + s$fixed_volume / s$final_volume else NA_real_

  list(
    feasible = feasible,
    message = if (feasible) {
      sprintf("Feasible: worst well uses %.1f%% with the suggested stocks.", 100 * worst)
    } else {
      "No set of stocks within max_stock_conc makes every well fit. Narrow the design ranges or reduce the fixed volume."
    },
    worst_fraction = worst,
    infinite_components = character(),
    stocks = data.frame(
      component = comps,
      units = stocks$units,
      stock_conc = stocks$stock_conc,
      max_stock_conc = stocks$max_stock_conc,
      suggested_stock = if (feasible) suggested else NA_real_,
      suggested_rounded = if (feasible) rounded else NA_real_,
      changed = if (feasible) rounded != stocks$stock_conc else NA,
      stringsAsFactors = FALSE
    )
  )
}
