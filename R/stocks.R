# Stock table: one row per component.
#
# Units are never converted: stock_conc, final_conc and the design values for a
# component must all be in the same units.

STOCK_COLUMNS <- c("component", "role", "units", "stock_conc", "final_conc",
                   "working_stock_conc", "max_stock_conc", "notes")
STOCK_NUMERIC <- c("stock_conc", "final_conc", "working_stock_conc", "max_stock_conc")

# Names the app uses for its own columns and sources; a component with one of
# these names would clash with them (compared ignoring case).
RESERVED_NAMES <- c("water", "diluent", "fixed", "fixed mix", "total", "plate", "well", "row",
                    "col", "run", "replicate", "position", "status", "fraction_used",
                    "largest_component", "below_min_sources")

#' Bring a stock data frame to the standard columns and types.
#'
#' Values that are not numbers (e.g. "40 mM") become NA; they are kept in
#' attr(, "unparsed") (component, column, value) so validate_stocks() can
#' name them.
normalise_stocks <- function(stocks) {
  unparsed <- attr(stocks, "unparsed")
  stocks <- as.data.frame(stocks, stringsAsFactors = FALSE)
  names(stocks) <- tolower(trimws(names(stocks)))
  for (col in setdiff(STOCK_COLUMNS, names(stocks))) stocks[[col]] <- rep(NA, nrow(stocks))
  stocks <- stocks[, STOCK_COLUMNS]
  stocks$component <- trimws(as.character(stocks$component))
  stocks$component[is.na(stocks$component)] <- ""
  stocks$role <- tolower(trimws(as.character(stocks$role)))
  stocks$role[is.na(stocks$role) | stocks$role == ""] <- "factor"
  stocks$units <- trimws(as.character(stocks$units))
  stocks$units[is.na(stocks$units)] <- ""
  stocks$notes <- trimws(as.character(stocks$notes))
  stocks$notes[is.na(stocks$notes)] <- ""
  for (col in STOCK_NUMERIC) {
    raw <- trimws(as.character(stocks[[col]]))
    stocks[[col]] <- parse_num(raw)
    bad <- !is.na(raw) & raw != "" & raw != "NA" & is.na(stocks[[col]])
    if (any(bad)) {
      unparsed <- rbind(unparsed, data.frame(component = stocks$component[bad], column = col,
                                             value = raw[bad], stringsAsFactors = FALSE))
    }
  }
  stocks <- stocks[stocks$component != "", , drop = FALSE]
  rownames(stocks) <- NULL
  if (!is.null(unparsed) && nrow(unparsed)) attr(stocks, "unparsed") <- unique(unparsed)
  stocks
}

read_stocks <- function(file) {
  normalise_stocks(read_csv_text(file, header = TRUE))
}

write_stocks <- function(stocks, file) {
  utils::write.csv(normalise_stocks(stocks), file, row.names = FALSE, na = "")
}

#' A stock table with one factor row per design component. Rows of `existing`
#' are kept (matched case-insensitively), as are its constant components.
default_stocks <- function(components, existing = NULL) {
  blank <- normalise_stocks(data.frame(component = components, role = "factor",
                                       stringsAsFactors = FALSE))
  if (is.null(existing) || !nrow(existing)) return(blank)
  existing <- normalise_stocks(existing)
  key <- tolower(existing$component)
  rows <- lapply(components, function(comp) {
    hit <- which(key == tolower(comp) & existing$role == "factor")
    if (length(hit)) existing[hit[1], ] else blank[blank$component == comp, ]
  })
  out <- do.call(rbind, c(rows, list(existing[existing$role == "constant", , drop = FALSE])))
  rownames(out) <- NULL
  out
}

#' Check a stock table against the design factors.
#'
#' Returns a data frame (component, severity, problem). Severity "error" stops
#' the calculation; "warning" is informational.
validate_stocks <- function(stocks, factors) {
  stocks <- normalise_stocks(stocks)
  probs <- list()
  add <- function(comp, sev, msg) probs[[length(probs) + 1]] <<-
    data.frame(component = comp, severity = sev, problem = msg, stringsAsFactors = FALSE)

  key <- tolower(stocks$component)
  for (comp in unique(stocks$component[duplicated(key)])) add(comp, "error", "listed more than once")
  for (i in which(!stocks$role %in% c("factor", "constant"))) {
    add(stocks$component[i], "error", sprintf("role '%s' must be factor or constant", stocks$role[i]))
  }

  names_used <- unique(c(factors, stocks$component[stocks$role == "constant"]))
  for (comp in names_used[tolower(names_used) %in% RESERVED_NAMES]) {
    add(comp, "error", sprintf("'%s' is a name used by the app's own columns; rename this component", comp))
  }

  fac_rows <- stocks$role == "factor"
  for (comp in factors[!tolower(factors) %in% key[fac_rows]]) {
    add(comp, "error", "design factor has no factor row in the stock table")
  }
  for (comp in stocks$component[fac_rows & !key %in% tolower(factors)]) {
    add(comp, "warning", "factor not in the design; ignored")
  }

  unparsed <- attr(stocks, "unparsed")
  label <- c(stock_conc = "stock conc.", final_conc = "final conc.",
             working_stock_conc = "working stock", max_stock_conc = "max stock")
  # A value that was entered but is not a number; NULL if none.
  not_number <- function(comp, col) {
    if (is.null(unparsed)) return(NULL)
    v <- unparsed$value[unparsed$component == comp & unparsed$column == col]
    if (length(v)) sprintf("%s '%s' is not a number", label[[col]], v[1])
  }

  for (i in seq_len(nrow(stocks))) {
    s <- stocks[i, ]
    nn <- not_number(s$component, "stock_conc")
    if (!is.null(nn)) add(s$component, "error", nn)
    else if (is.na(s$stock_conc)) add(s$component, "error", "stock conc. is missing")
    else if (s$stock_conc <= 0) add(s$component, "error", "stock conc. must be above 0")

    if (s$role == "constant") {
      nn <- not_number(s$component, "final_conc")
      if (!is.null(nn)) add(s$component, "error", nn)
      else if (is.na(s$final_conc)) add(s$component, "error", "constant needs a final conc.")
      else if (s$final_conc < 0) add(s$component, "error", "final conc. cannot be negative")
      else if (!is.na(s$stock_conc) && s$final_conc > s$stock_conc)
        add(s$component, "error", "final conc. is above the stock conc.")
    }

    nn <- not_number(s$component, "working_stock_conc")
    if (!is.null(nn)) add(s$component, "error", nn)
    else if (!is.na(s$working_stock_conc)) {
      if (s$working_stock_conc <= 0) add(s$component, "error", "working stock must be above 0")
      else if (!is.na(s$stock_conc) && s$working_stock_conc >= s$stock_conc)
        add(s$component, "warning", "working stock is not more dilute than the stock")
    }

    nn <- not_number(s$component, "max_stock_conc")
    if (!is.null(nn)) add(s$component, "error", nn)
    else if (!is.na(s$max_stock_conc)) {
      if (s$max_stock_conc <= 0) add(s$component, "error", "max stock must be above 0")
      else if (!is.na(s$stock_conc) && s$stock_conc > s$max_stock_conc)
        add(s$component, "warning", "stock conc. is above its max stock")
    }
  }

  if (!length(probs)) {
    return(data.frame(component = character(), severity = character(),
                      problem = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, probs)
}

#' Stock rows actually used: design factors (renamed to the design's spelling)
#' followed by constants.
active_stocks <- function(stocks, factors) {
  stocks <- normalise_stocks(stocks)
  fac <-stocks[stocks$role == "factor", , drop = FALSE]
  fac <- fac[match(tolower(factors), tolower(fac$component)), , drop = FALSE]
  fac$component <- factors
  out <- rbind(fac, stocks[stocks$role == "constant", , drop = FALSE])
  rownames(out) <- NULL
  out
}
