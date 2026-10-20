asr4 <- function(...) build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"), ...)

test_that("default order of addition: water, constants, salts, additives, viscous", {
  r <- asr4()
  expect_equal(r$addition_order,
               c("Water", "Tris-HCl pH 8.3", "MgCl2", "Kcl", "CaCl2",
                 "Betaine", "Trehalose", "TCEP", "Tween20", "PEG8000"))
})

test_that("picklist follows the order, drops zeros and skips failed wells", {
  r <- asr4()
  p <- r$picklist
  expect_equal(names(p), c("step", "source", "destination_plate", "destination_well", "volume_uL"))
  expect_true(all(p$volume_uL > 0))
  expect_false(is.unsorted(p$step))
  expect_equal(unique(p$source), r$addition_order)
  failed <- r$volumes$well[r$volumes$status == "OVER VOLUME"]
  expect_false(any(p$destination_well %in% failed))
  expect_equal(sum(p$source == "Tris-HCl pH 8.3"), 70)

  with_failed <- asr4(include_failed_in_picklist = TRUE)$picklist
  expect_equal(sum(with_failed$source == "Tris-HCl pH 8.3"), 88)
})

test_that("custom order of addition is respected", {
  r <- asr4(addition_order = c("PEG8000", "Water"))
  expect_equal(r$addition_order[1:2], c("PEG8000", "Water"))
  expect_equal(r$picklist$source[1], "PEG8000")
  expect_setequal(r$addition_order, asr4()$addition_order)
})

test_that("stock CSV round-trips", {
  s <- read_stocks(fixture("ASR4_stocks.csv"))
  f <- tempfile(fileext = ".csv")
  write_stocks(s, f)
  expect_equal(read_stocks(f), s)
  expect_equal(names(s), STOCK_COLUMNS)
})

test_that("default stocks keep existing rows and constants", {
  s <- default_stocks(c("MgCl2", "KCL", "NewSalt"), read_stocks(fixture("ASR4_stocks.csv")))
  expect_equal(s$component, c("MgCl2", "Kcl", "NewSalt", "Tris-HCl pH 8.3"))
  expect_equal(s$stock_conc, c(1000, 2000, NA, 1000))
})

test_that("Excel export has every sheet and exactly one selected tab", {
  r <- asr4()
  f <- tempfile(fileext = ".xlsx")
  export_excel(r, f)
  expect_equal(openxlsx::getSheetNames(f),
               c("Summary", "Settings", "Stocks", "Volumes", "Stock prep", "Picklist",
                 "Failing wells", "Min stocks", "Design"))
  dir <- tempfile()
  utils::unzip(f, exdir = dir)
  sheets <- list.files(file.path(dir, "xl", "worksheets"), pattern = "\\.xml$", full.names = TRUE)
  selected <- vapply(sheets, function(x) grepl('tabSelected="(1|true)"', paste(readLines(x, warn = FALSE), collapse = "")), logical(1))
  expect_equal(sum(selected), 1)
  expect_true(selected[basename(sheets) == "sheet1.xml"])
  vol <- openxlsx::read.xlsx(f, "Volumes")
  expect_equal(nrow(vol), 88)
})

test_that("plate maps build for components, water and fraction used", {
  r <- asr4()
  expect_true(all(c("PEG8000", "water", "fraction_used") %in% plate_map_choices(r)))
  for (v in c("PEG8000", "water", "fraction_used")) expect_s3_class(plot_plate_map(r, v), "ggplot")
  r384 <- asr4(settings = list(plate_format = 384))
  expect_s3_class(plot_plate_map(r384, "Kcl"), "ggplot")
})
