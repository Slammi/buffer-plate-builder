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
#' whose stock must not change.
optimise_stocks <- function(result, hold = character()) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) stop("Package lpSolve is needed for the optimiser.")
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

  I <- diag(n)
  mat <- rbind(C, I, I)
  dir <- c(rep("<=", nrow(C)), rep("<=", n), rep(">=", n))
  rhs <- c(rep(rhs_fit, nrow(C)), upper, lower)
  sol <- lpSolve::lp("max", stocks$stock_conc, mat, dir, rhs)

  feasible <- sol$status == 0
  x <- if (feasible) sol$solution else rep(NA_real_, n)
  suggested <- ifelse(abs(x - upper) <= 1e-12 * pmax(upper, 1), stocks$stock_conc, 1 / x)
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
