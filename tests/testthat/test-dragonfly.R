# Dragonfly dispense file, compared with the reference "ASR4 Screening DFD.csv"
# (PEG30 design, 50 µL final volume, no constants).

dfd_result <- function(...) {
  build_plate(fixture("ASR4_design_PEG30_transposed.csv"), fixture("ASR4_stocks_DFD.csv"),
              settings = list(final_volume = 50, ...))
}
design_order <- c("Water", "MgCl2", "Kcl", "PEG8000", "Betaine", "Trehalose", "TCEP", "CaCl2", "Tween20")

test_that("DFD matches the reference file line for line", {
  r <- dfd_result()
  ref <- sub("\r$", "", readLines(fixture("ASR4_Screening_DFD.csv"), warn = FALSE))
  ours <- dragonfly_dfd(r, order = design_order)
  expect_equal(length(ours), length(ref))

  is_row <- grepl(DFD_ROW_PATTERN, ref)
  expect_equal(grepl(DFD_ROW_PATTERN, ours), is_row)
  # Header, labels and blank lines are identical.
  expect_equal(ours[!is_row], ref[!is_row])
  # Volumes agree to the 5th decimal (the spreadsheet rounded diluent to 5 dp).
  num <- function(x) as.numeric(unlist(strsplit(trimws(x), "\\s*,\\s*")))
  expect_equal(num(ours[is_row]), num(ref[is_row]), tolerance = 1e-5, scale = 1)
})

test_that("labels use the stock table's spelling, stock and units", {
  r <- dfd_result()
  expect_equal(dfd_label("Kcl", r), "KCl - 2000 mM")
  expect_equal(dfd_label("PEG8000", r), "PEG8000 - 60 %v/v")
  expect_equal(dfd_label("Water", r), "Diluent")
})

test_that("written file has CRLF line endings and reads back", {
  r <- dfd_result()
  f <- tempfile(fileext = ".csv")
  write_dfd(dragonfly_dfd(r, order = design_order), f)
  bytes <- readBin(f, "raw", file.size(f))
  expect_equal(rawToChar(bytes[1:15]), "version, '1.2\r\n")
  expect_equal(read_dfd(f), read_dfd(fixture("ASR4_Screening_DFD.csv")), tolerance = 1e-5)
})

test_that("default block order follows the order of addition", {
  r <- dfd_result()
  labels <- dragonfly_dfd(r)
  labels <- labels[!grepl(DFD_ROW_PATTERN, labels) & nzchar(labels)][-(1:2)]
  expect_equal(labels[1], "Diluent")
  expect_equal(tail(labels, 1), "PEG8000 - 60 %v/v")
})

test_that("wells without a run get diluent only when fill_unused is on", {
  r <- dfd_result()
  h5 <- function(lines) {
    d <- read_dfd({f <- tempfile(); write_dfd(lines, f); f})
    d$volume[d$source == "Diluent" & d$row == 8 & d$col == 5]
  }
  expect_equal(h5(dragonfly_dfd(r)), 50)
  expect_equal(h5(dragonfly_dfd(r, fill_unused = FALSE)), 0)
})

test_that("OVER VOLUME wells are left empty unless included", {
  r <- build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"))
  bad <- r$volumes[r$volumes$status == "OVER VOLUME", ][1, ]
  get <- function(lines, src) {
    f <- tempfile(); write_dfd(lines, f); d <- read_dfd(f)
    d$volume[startsWith(d$source, src) & d$row == bad$row & d$col == bad$col]
  }
  expect_equal(get(dragonfly_dfd(r), "PEG8000"), 0)
  expect_equal(get(dragonfly_dfd(r), "Diluent"), 0)
  expect_equal(get(dragonfly_dfd(r, include_failed = TRUE), "PEG8000"), bad$PEG8000)
  expect_equal(get(dragonfly_dfd(r, include_failed = TRUE), "Diluent"), 0)
})

test_that("one file per plate, 384-well grids, working stocks and no fixed mix", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$working_stock_conc[stocks$component == "TCEP"] <- 50
  stocks$stock_conc[stocks$component == "TCEP"] <- 5000
  r <- build_plate(fixture("ASR4_design_PEG24.csv"), stocks,
                   settings = list(replicates = 2, fixed_volume = 5))
  expect_equal(max(r$volumes$plate), 2)
  p2 <- dragonfly_dfd(r, plate = 2)
  expect_true("TCEP - 50 mM" %in% p2)
  expect_true("TCEP - 5000 mM" %in% p2)
  expect_false(any(grepl("Fixed", p2)))
  expect_error(dragonfly_dfd(r, plate = 3), "No runs on plate 3")

  r384 <- build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"),
                      settings = list(plate_format = 384))
  rows <- grep(DFD_ROW_PATTERN, dragonfly_dfd(r384), value = TRUE)
  expect_equal(length(rows) %% 16, 0)
  expect_equal(length(strsplit(rows[1], ",")[[1]]), 24)
})
