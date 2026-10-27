# Stock table: one row per component.
#
# Units are never converted: stock_conc, final_conc and the design values for a
# component must all be in the same units.

STOCK_COLUMNS <- c("component", "role", "units", "stock_conc", "final_conc",
                   "working_stock_conc", "max_stock_conc", "notes")
STOCK_NUMERIC <- c("stock_conc", "final_conc", "working_stock_conc", "max_stock_conc")

#' Bring a stock data frame to the standard columns and types.
normalise_stocks <- function(stocks) {
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
  for (col in STOCK_NUMERIC) stocks[[col]] <- parse_num(stocks[[col]])
  stocks <- stocks[stocks$component != "", , drop = FALSE]
  rownames(stocks) <- NULL
  stocks
}

read_stocks <- function(file) {
  normalise_stocks(utils::read.csv(file, colClasses = "character", check.names = FALSE,
                                   strip.white = TRUE, fileEncoding = "UTF-8-BOM"))
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

  fac_rows <- stocks$role == "factor"
  for (comp in factors[!tolower(factors) %in% key[fac_rows]]) {
    add(comp, "error", "design factor has no factor row in the stock table")
  }
  for (comp in stocks$component[fac_rows & !key %in% tolower(factors)]) {
    add(comp, "warning", "factor not in the design; ignored")
  }

  for (i in seq_len(nrow(stocks))) {
    s <- stocks[i, ]
    if (is.na(s$stock_conc) || s$stock_conc <= 0) add(s$component, "error", "stock_conc must be > 0")
    if (s$role == "constant") {
      if (is.na(s$final_conc) || s$final_conc < 0) {
        add(s$component, "error", "constant needs final_conc")
      } else if (!is.na(s$stock_conc) && s$final_conc > s$stock_conc) {
        add(s$component, "error", "final_conc is above stock_conc")
      }
    }
    if (!is.na(s$working_stock_conc)) {
      if (s$working_stock_conc <= 0) add(s$component, "error", "working_stock_conc must be > 0")
      else if (!is.na(s$stock_conc) && s$working_stock_conc >= s$stock_conc)
        add(s$component, "warning", "working stock is not more dilute than the stock")
    }
    if (!is.na(s$max_stock_conc) && !is.na(s$stock_conc) && s$stock_conc > s$max_stock_conc) {
      add(s$component, "warning", "stock_conc is above max_stock_conc")
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
