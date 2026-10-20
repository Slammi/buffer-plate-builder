# Top-level entry point: design + stocks + settings -> every output.

#' Build the plate.
#'
#' @param design_csv Path to the design CSV, or a "design" object from read_design().
#' @param stocks_csv Path to the stock CSV, or a stock data frame.
#' @param settings   Named list overriding DEFAULT_SETTINGS.
#' @param orientation "auto", "runs_in_rows" or "runs_in_columns".
#' @param components Design columns to treat as components (default: auto-detected).
#' @param addition_order Source names in order of addition (default: default_addition_order()).
#' @param include_failed_in_picklist Keep OVER VOLUME wells in the picklist.
#' @return A list: settings, design, design_matrix, stocks, problems, layout,
#'   concentrations, runs, volumes, transfers, summary, failing, min_stocks,
#'   stock_prep, addition_order, picklist.
build_plate <- function(design_csv, stocks_csv, settings = list(), orientation = "auto",
                        components = NULL, addition_order = NULL,
                        include_failed_in_picklist = FALSE) {
  s <- resolve_settings(settings)
  design <- if (inherits(design_csv, "design")) design_csv else read_design(design_csv, orientation)
  factors <- if (is.null(components)) design$components else components
  dmat <- design_matrix(design, factors)

  stocks_in <- if (is.data.frame(stocks_csv)) normalise_stocks(stocks_csv) else read_stocks(stocks_csv)
  problems <- validate_stocks(stocks_in, factors)
  if (any(problems$severity == "error")) {
    err <- problems[problems$severity == "error", ]
    stop("Stock table problems:\n", paste0("  ", err$component, ": ", err$problem, collapse = "\n"),
         call. = FALSE)
  }
  stocks <- active_stocks(stocks_in, factors)

  layout <- layout_runs(nrow(dmat), s)
  conc <- run_concentrations(dmat, stocks)
  cv <- component_volumes(conc, s)
  runs <- summarise_runs(cv, nrow(dmat), s)

  sources <- unlist(lapply(stocks$component, function(cmp) {
    intersect(c(cmp, paste(cmp, "(working)")), unique(cv$source))
  }))
  volumes <- volumes_table(layout, cv, runs, sources)
  tr <- transfers(volumes, sources)

  all_sources <- c("Water", sources, if (s$fixed_volume > 0) "Fixed mix")
  order <- if (is.null(addition_order)) default_addition_order(all_sources, stocks) else {
    c(intersect(addition_order, all_sources), setdiff(all_sources, addition_order))
  }

  list(
    settings = s,
    design = design,
    design_matrix = dmat,
    stocks = stocks,
    problems = problems,
    layout = layout,
    concentrations = cv,
    runs = runs,
    volumes = volumes,
    transfers = tr,
    summary = feasibility_summary(runs, volumes, s),
    failing = failing_wells(runs, volumes),
    min_stocks = min_stocks(cv, stocks, s),
    stock_prep = stock_prep(tr, order, stocks, s),
    addition_order = order,
    picklist = picklist(tr, order, include_failed_in_picklist)
  )
}
