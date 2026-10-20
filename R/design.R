# Reading DoE design CSVs.
#
# Default layout: one row per run, one column per component (header row of names).
# Transposed layout: one row per component, runs across (first column of names).

# Columns that look numeric but are run bookkeeping, not components.
RUN_ID_PATTERN <- "^(run|runs|run ?order|run ?no\\.?|run ?#|std ?order|standard ?order|block|pattern|id)$"

parse_num <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

read_raw_csv <- function(path) {
  raw <- utils::read.csv(path, header = FALSE, colClasses = "character",
                         na.strings = character(), check.names = FALSE,
                         strip.white = TRUE, fill = TRUE,
                         fileEncoding = "UTF-8-BOM")
  raw[] <- lapply(raw, function(x) trimws(ifelse(is.na(x), "", x)))
  raw
}

# A label is a non-blank cell that is not a number.
is_label <- function(x) {
  x <- trimws(as.character(x))
  x != "" & is.na(parse_num(x))
}

#' Guess whether runs are in rows (names across the first row) or in columns
#' (names down the first column).
detect_orientation <- function(raw) {
  row_score <- mean(is_label(unlist(raw[1, ], use.names = FALSE)))
  col_score <- mean(is_label(raw[[1]]))
  if (col_score > row_score) "runs_in_columns" else "runs_in_rows"
}

#' Read a design CSV (path or a character data frame as read with header = FALSE).
#'
#' Returns an object of class "design" with the raw table (runs in rows, all
#' character), the column names, the auto-detected component columns and the
#' orientation used.
read_design <- function(file, orientation = c("auto", "runs_in_rows", "runs_in_columns")) {
  orientation <- match.arg(orientation)
  raw <- if (is.data.frame(file)) file else read_raw_csv(file)
  raw[] <- lapply(raw, function(x) trimws(ifelse(is.na(x), "", as.character(x))))

  detected <- detect_orientation(raw)
  used <- if (orientation == "auto") detected else orientation
  if (used == "runs_in_columns") {
    raw <- as.data.frame(t(as.matrix(raw)), stringsAsFactors = FALSE)
  }

  header <- trimws(unlist(raw[1, ], use.names = FALSE))
  body <- raw[-1, , drop = FALSE]
  keep <- header != ""
  body <- body[, keep, drop = FALSE]
  header <- header[keep]
  if (anyDuplicated(tolower(header))) {
    stop("Duplicate column names in design: ",
         paste(unique(header[duplicated(tolower(header))]), collapse = ", "),
         " (is the orientation right?)")
  }
  names(body) <- header
  body <- body[rowSums(body != "") > 0, , drop = FALSE]
  rownames(body) <- NULL

  numeric_col <- vapply(body, function(x) {
    filled <- x != ""
    any(filled) && all(!is.na(parse_num(x[filled])))
  }, logical(1))
  run_id <- grepl(RUN_ID_PATTERN, tolower(header))

  structure(
    list(table = body,
         columns = header,
         components = header[numeric_col & !run_id],
         orientation = used,
         detected_orientation = detected),
    class = "design"
  )
}

#' Numeric component matrix of a design: one row per run (in design order),
#' with a `run` column. Blank component cells are treated as 0.
design_matrix <- function(design, components = design$components) {
  missing <- setdiff(components, design$columns)
  if (length(missing)) stop("Not columns of the design: ", paste(missing, collapse = ", "))
  if (!length(components)) stop("No component columns selected.")

  tab <- design$table[, components, drop = FALSE]
  has_value <- rowSums(tab != "") > 0
  tab <- tab[has_value, , drop = FALSE]
  out <- as.data.frame(lapply(tab, parse_num), check.names = FALSE)

  bad <- vapply(tab, function(x) any(x != "" & is.na(parse_num(x))), logical(1))
  if (any(bad)) stop("Non-numeric values in component column(s): ",
                     paste(components[bad], collapse = ", "))
  if (anyNA(out)) {
    warning("Blank cells in component columns treated as 0: ",
            paste(components[colSums(is.na(out)) > 0], collapse = ", "), call. = FALSE)
    out[is.na(out)] <- 0
  }
  if (any(out < 0)) stop("Negative concentrations in the design.")
  cbind(run = seq_len(nrow(out)), out)
}

print.design <- function(x, ...) {
  cat(sprintf("Design: %d runs, %d columns (%s)\n", nrow(x$table), length(x$columns), x$orientation))
  cat("Components:", paste(x$components, collapse = ", "), "\n")
  other <- setdiff(x$columns, x$components)
  if (length(other)) cat("Excluded:  ", paste(other, collapse = ", "), "\n")
  invisible(x)
}
