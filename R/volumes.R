# Per-well volumes.
#
#   V_i   = C_final_i / C_stock_i * V_total      (factors and constants)
#   water = V_total - fixed - sum(V_i)
#
# A well is OVER VOLUME if water < 0, and BELOW MIN if any non-zero volume
# (water included) is under the minimum pipettable volume. Levels that would be
# below the minimum are taken from the working stock when one is defined.

VOL_TOL <- 1e-9

#' Concentration of every component in every run (long format).
run_concentrations <- function(dmat, stocks) {
  rows <- lapply(seq_len(nrow(stocks)), function(i) {
    s <- stocks[i, ]
    conc <- if (s$role == "factor") dmat[[s$component]] else rep(s$final_conc, nrow(dmat))
    data.frame(run = dmat$run, component = s$component, role = s$role, conc = conc,
               stock_conc = s$stock_conc, working_stock_conc = s$working_stock_conc,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Component volumes per run, choosing main or working stock per level.
component_volumes <- function(conc, settings) {
  V <- settings$final_volume
  v_main <- conc$conc / conc$stock_conc * V
  use_work <- !is.na(conc$working_stock_conc) & v_main > VOL_TOL &
    v_main < settings$min_volume - VOL_TOL
  conc$stock_used <- ifelse(use_work, conc$working_stock_conc, conc$stock_conc)
  conc$source <- ifelse(use_work, paste(conc$component, "(working)"), conc$component)
  conc$volume <- conc$conc / conc$stock_used * V
  conc
}

#' One row per run: totals, water, fraction used before water and status.
summarise_runs <- function(cv, n_runs, settings) {
  V <- settings$final_volume
  fixed <- settings$fixed_volume
  min_v <- settings$min_volume
  by_run <- split(cv, factor(cv$run, levels = seq_len(n_runs)))
  rows <- lapply(seq_len(n_runs), function(r) {
    x <- by_run[[r]]
    comp_total <- sum(x$volume)
    water <- V - fixed - comp_total
    small <- x$volume > VOL_TOL & x$volume < min_v - VOL_TOL
    water_small <- water > VOL_TOL & water < min_v - VOL_TOL
    fixed_small <- fixed > VOL_TOL & fixed < min_v - VOL_TOL
    big <- if (nrow(x) && any(x$volume > 0)) which.max(x$volume) else NA
    data.frame(
      run = r,
      component_total = comp_total,
      fixed = fixed,
      water = water,
      fraction_used = (comp_total + fixed) / V,
      over_volume = water < -VOL_TOL,
      below_min = any(small) || water_small || fixed_small,
      below_min_sources = paste(c(x$source[small], if (water_small) "Water",
                                  if (fixed_small) "Fixed mix"), collapse = ", "),
      largest_component = if (is.na(big)) NA_character_ else x$component[big],
      largest_volume = if (is.na(big)) 0 else x$volume[big],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$status <- ifelse(out$over_volume, "OVER VOLUME", ifelse(out$below_min, "BELOW MIN", "OK"))
  out
}

#' Wide per-well volumes table: run, plate, well, one column per source, fixed,
#' water, total, fraction used and status.
volumes_table <- function(layout, cv, runs, sources) {
  wide <- stats::xtabs(volume ~ run + source, data = cv)
  wide <- as.data.frame.matrix(wide)[, sources, drop = FALSE]
  wide$run <- as.integer(rownames(wide))
  layout$position <- seq_len(nrow(layout))
  tab <- merge(layout, wide, by = "run")
  tab <- merge(tab, runs[, c("run", "fixed", "water", "fraction_used", "status",
                             "largest_component", "below_min_sources")], by = "run")
  tab$total <- rowSums(tab[, sources, drop = FALSE]) + tab$fixed + pmax(tab$water, 0)
  tab <- tab[order(tab$position), ]
  tab <- tab[, c("run", "replicate", "plate", "well", "row", "col", sources,
                 "fixed", "water", "total", "fraction_used", "status",
                 "largest_component", "below_min_sources")]
  rownames(tab) <- NULL
  tab
}

#' Every liquid transfer: one row per well x source with volume > 0, including
#' water and the fixed mix.
transfers <- function(volumes, sources) {
  all_src <- c(sources, "fixed", "water")
  rows <- lapply(all_src, function(src) {
    v <- volumes[[src]]
    data.frame(run = volumes$run, replicate = volumes$replicate, plate = volumes$plate,
               well = volumes$well, row = volumes$row, col = volumes$col,
               source = switch(src, fixed = "Fixed mix", water = "Water", src),
               volume = v, status = volumes$status, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[out$volume > VOL_TOL, , drop = FALSE]
}
