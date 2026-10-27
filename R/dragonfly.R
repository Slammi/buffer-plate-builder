# Dragonfly dispense file (DFD), matching the layout of "ASR4 Screening DFD.csv":
#
#   version, '1.2
#   TreatAsVolumes, true
#   <blank>
#   Diluent
#    v, v, ... (one line per plate row, one value per column, µL, 6 decimals)
#   <blank>
#   MgCl2 - 1000 mM
#    ...
#
# One block per source in order of addition; water is "Diluent". The fixed
# mix is not dispensed by the Dragonfly and is left out. Lines end in CRLF.

DFD_HEADER <- c("version, '1.2", "TreatAsVolumes, true", "")

# Format a stock concentration without scientific notation or trailing zeros.
fmt_conc <- function(x) format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)

#' Block label for a source: "Diluent" for water, otherwise
#' "<name as in the stock table> - <stock conc> <units>".
dfd_label <- function(source, result) {
  if (source == "Water") return("Diluent")
  working <- grepl(" \\(working\\)$", source)
  comp <- sub(" \\(working\\)$", "", source)
  tab <- result$stock_table
  i <- match(tolower(comp), tolower(tab$component))
  conc <- if (working) tab$working_stock_conc[i] else tab$stock_conc[i]
  trimws(sprintf("%s - %s %s", tab$component[i], fmt_conc(conc), tab$units[i]))
}

#' Lines of the DFD file for one plate.
#'
#' @param order Sources in order of addition (default: result$addition_order).
#' @param include_failed Dispense OVER VOLUME wells anyway (diluent 0). By
#'   default those wells are left empty, as in the picklist.
#' @param fill_unused Fill wells without a run with diluent to the final volume.
dragonfly_dfd <- function(result, plate = 1, order = result$addition_order,
                          include_failed = FALSE, fill_unused = TRUE) {
  s <- result$settings
  d <- plate_dims(s$plate_format)
  vol <- result$volumes[result$volumes$plate == plate, , drop = FALSE]
  if (!nrow(vol)) stop("No runs on plate ", plate)
  skip <- if (include_failed) rep(FALSE, nrow(vol)) else vol$status == "OVER VOLUME"
  sources <- setdiff(order, "Fixed mix")

  grid_for <- function(src) {
    g <- matrix(0, d[["rows"]], d[["cols"]])
    if (src == "Water" && fill_unused) g[] <- s$final_volume
    v <- if (src == "Water") pmax(vol$water, 0) else vol[[src]]
    v[skip] <- 0
    g[cbind(vol$row, vol$col)] <- v
    g
  }

  blocks <- lapply(seq_along(sources), function(k) {
    g <- grid_for(sources[k])
    rows <- apply(g, 1, function(r) paste0(" ", paste(sprintf("%.6f", r), collapse = ", ")))
    # Spacing as in the reference file: one blank line after the first and
    # last blocks, two after the others.
    gap <- if (k == 1 || k == length(sources)) "" else c("", "")
    c(dfd_label(sources[k], result), rows, gap)
  })
  c(DFD_HEADER, unlist(blocks))
}

#' Write DFD lines with Windows (CRLF) line endings.
write_dfd <- function(lines, file) {
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeLines(lines, con, sep = "\r\n", useBytes = TRUE)
  invisible(file)
}

DFD_ROW_PATTERN <- "^\\s*-?[0-9.]+(\\s*,\\s*-?[0-9.]+)*\\s*$"

#' Read a DFD file back into a long data frame (source, row, col, volume).
read_dfd <- function(file) {
  lines <- sub("\r$", "", readLines(file, warn = FALSE))
  body <- lines[-(1:2)]
  label <- NA_character_
  row <- 0
  out <- list()
  for (ln in body) {
    if (!nzchar(trimws(ln))) next
    if (!grepl(DFD_ROW_PATTERN, ln)) {
      label <- trimws(ln)
      row <- 0
      next
    }
    row <- row + 1
    v <- as.numeric(strsplit(trimws(ln), "\\s*,\\s*")[[1]])
    out[[length(out) + 1]] <- data.frame(source = label, row = row, col = seq_along(v),
                                         volume = v, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}
