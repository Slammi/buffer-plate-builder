# Per-component checks shown on the Inputs page (mirrors the Inputs tab of the
# ASR4 Excel calculator).

#' One row per design factor: design range, volume and fraction of the well at
#' the maximum level, volume at the smallest non-zero level, and a pipetting
#' check. `status` is "ok", "warn" or "bad" (for colouring).
factor_checks <- function(dmat, stocks, settings) {
  s <- resolve_settings(settings)
  V <- s$final_volume
  stocks <- normalise_stocks(stocks)
  comps <- setdiff(names(dmat), "run")
  rows <- lapply(comps, function(comp) {
    i <- match(tolower(comp), tolower(stocks$component[stocks$role == "factor"]))
    st <- if (is.na(i)) stocks[0, ][NA_integer_, ] else stocks[stocks$role == "factor", ][i, ]
    lv <- dmat[[comp]]
    small <- if (any(lv > 0)) min(lv[lv > 0]) else NA_real_
    stock <- st$stock_conc
    vol_small <- small / stock * V
    vol_small_work <- small / st$working_stock_conc * V
    frac_max <- max(lv) / stock
    chk <- if (is.na(stock) || stock <= 0) {
      c("Enter stock conc.", "warn")
    } else if (!is.na(st$working_stock_conc) && st$working_stock_conc <= 0) {
      c("Working stock must be above 0", "bad")
    } else if (frac_max > 1 - s$fixed_volume / V) {
      c("Does not fit at max, even alone", "bad")
    } else if (!is.na(vol_small) && vol_small < s$min_volume - VOL_TOL) {
      if (is.na(st$working_stock_conc)) c("Below min - use a diluted working stock", "warn")
      else if (vol_small_work < s$min_volume - VOL_TOL) c("Below min even with working stock", "warn")
      else if (frac_max > 0.5) c("Uses >50% of well at max", "warn")
      else c("OK (low levels from working stock)", "ok")
    } else if (frac_max > 0.5) {
      c("Uses >50% of well at max", "warn")
    } else {
      c("OK", "ok")
    }
    data.frame(component = comp, units = st$units, stock_conc = stock,
               design_min = min(lv), design_max = max(lv),
               vol_at_max = max(lv) / stock * V, fraction_at_max = frac_max,
               smallest_level = small, vol_at_smallest = vol_small,
               check = chk[1], status = chk[2],
               working_stock_conc = st$working_stock_conc, max_stock_conc = st$max_stock_conc,
               notes = st$notes, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' One row per constant component: volume and fraction of the well, and a
#' pipetting check.
constant_checks <- function(stocks, settings) {
  s <- resolve_settings(settings)
  V <- s$final_volume
  stocks <- normalise_stocks(stocks)
  cst <- stocks[stocks$role == "constant", , drop = FALSE]
  vol <- cst$final_conc / cst$stock_conc * V
  chk <- vapply(seq_len(nrow(cst)), function(i) {
    if (is.na(cst$stock_conc[i]) || is.na(cst$final_conc[i]) || cst$stock_conc[i] <= 0) "Enter stock and final conc.|warn"
    else if (cst$final_conc[i] < 0) "Final conc. cannot be negative|bad"
    else if (cst$final_conc[i] > cst$stock_conc[i]) "Final conc. exceeds stock|bad"
    else if (vol[i] > VOL_TOL && vol[i] < s$min_volume - VOL_TOL) "Below min - use a diluted working stock|warn"
    else "OK|ok"
  }, character(1))
  data.frame(component = cst$component, units = cst$units, stock_conc = cst$stock_conc,
             final_conc = cst$final_conc, vol_per_well = vol, fraction_of_well = vol / V,
             check = sub("\\|.*", "", chk), status = sub(".*\\|", "", chk),
             notes = cst$notes, stringsAsFactors = FALSE)
}
