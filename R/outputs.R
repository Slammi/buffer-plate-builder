# Stock preparation, picklist and Excel export.

SALT_PATTERN <- "(cl[23]?$|chloride|acetate|oac2?$|sulfate|sulphate|so4$|phosphate|po4$|nitrate|glutamate)"
VISCOUS_PATTERN <- "(peg|glycerol|ficoll|dextran|sucrose|polyethylene)"

#' Group used for the default order of addition.
addition_group <- function(source, stocks) {
  comp <- sub(" \\(working\\)$", "", source)
  role <- stocks$role[match(tolower(comp), tolower(stocks$component))]
  key <- tolower(gsub("[^A-Za-z0-9]", "", comp))
  ifelse(source == "Water", 1L,
  ifelse(source == "Fixed mix", 6L,
  ifelse(!is.na(role) & role == "constant", 2L,
  ifelse(grepl(VISCOUS_PATTERN, key), 5L,
  ifelse(grepl(SALT_PATTERN, key), 3L, 4L)))))
}

#' Default order of addition: water, constant buffers, salts, other additives,
#' viscous components (PEG etc.), fixed mix last. Ties keep stock-table order.
default_addition_order <- function(sources, stocks) {
  sources[order(addition_group(sources, stocks), seq_along(sources))]
}

#' Per source: wells receiving it, total dispensed, overage, dead volume and
#' the total volume to prepare.
stock_prep <- function(tr, sources, stocks, settings) {
  units <- stocks$units[match(tolower(sub(" \\(working\\)$", "", sources)), tolower(stocks$component))]
  conc <- vapply(sources, function(src) {
    i <- match(tolower(sub(" \\(working\\)$", "", src)), tolower(stocks$component))
    if (is.na(i)) NA_real_
    else if (grepl(" \\(working\\)$", src)) stocks$working_stock_conc[i]
    else stocks$stock_conc[i]
  }, numeric(1))
  rows <- lapply(seq_along(sources), function(k) {
    v <- tr$volume[tr$source == sources[k]]
    total <- sum(v)
    data.frame(source = sources[k], stock_conc = unname(conc[k]),
               units = if (is.na(units[k])) "" else units[k],
               wells = length(v), total_dispensed_uL = total,
               overage_uL = total * settings$overage,
               dead_volume_uL = if (length(v)) settings$dead_volume else 0,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$total_to_prepare_uL <- out$total_dispensed_uL + out$overage_uL + out$dead_volume_uL
  out
}

#' Picklist in order of addition: step, source, destination plate/well, volume.
#' Zero volumes are dropped; OVER VOLUME wells are skipped unless
#' include_failed = TRUE.
picklist <- function(tr, order, include_failed = FALSE) {
  if (!include_failed) tr <- tr[tr$status != "OVER VOLUME", , drop = FALSE]
  missing <- setdiff(unique(tr$source), order)
  order <- c(order, missing)
  tr$step <- match(tr$source, order)
  tr <- tr[order(tr$step, tr$plate, tr$row, tr$col), ]
  out <- data.frame(step = tr$step, source = tr$source, destination_plate = tr$plate,
                    destination_well = tr$well, volume_uL = round(tr$volume, 3),
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Write every output to one workbook. Exactly one sheet (the first) is
#' selected, so the file never opens with grouped tabs.
export_excel <- function(result, file, picklist_table = result$picklist) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package openxlsx is needed for Excel export.")
  s <- result$settings
  settings_tab <- data.frame(
    setting = c("Final well volume (uL)", "Fixed volume per well (uL)", "Minimum pipettable volume (uL)",
                "Stock overage (fraction)", "Dead volume per reservoir (uL)", "Plate format",
                "Fill direction", "Exclude edge wells", "Reserved wells", "Randomize run order",
                "Seed", "Replicates per run"),
    value = c(s$final_volume, s$fixed_volume, s$min_volume, s$overage, s$dead_volume,
              s$plate_format, s$fill, s$exclude_edges,
              paste(normalise_wells(s$reserved_wells), collapse = " "),
              s$randomize, s$seed, s$replicates),
    stringsAsFactors = FALSE
  )
  sheets <- list(
    Summary = result$summary,
    Settings = settings_tab,
    Stocks = result$stocks,
    Volumes = result$volumes,
    `Stock prep` = result$stock_prep,
    Picklist = picklist_table,
    `Failing wells` = result$failing,
    `Min stocks` = result$min_stocks,
    Design = cbind(run = seq_len(nrow(result$design_matrix)), result$design_matrix[, -1, drop = FALSE])
  )
  wb <- openxlsx::createWorkbook()
  head_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]], headerStyle = head_style)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(sheets[[nm]])), widths = "auto")
  }
  select_single_sheet(wb, 1)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Make sheet `k` the active, and only selected, sheet. openxlsx writes
# tabSelected on the active sheet only when saving. (Editing the sheetViews XML
# by hand once matched "<sheetViews>" and made Excel repair the file.)
select_single_sheet <- function(wb, k = 1) {
  openxlsx::activeSheet(wb) <- k
  invisible(wb)
}
