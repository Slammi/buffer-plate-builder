test_that("runs-in-rows design is detected and response columns excluded", {
  d <- read_design(fixture("ASR4_design_PEG24.csv"))
  expect_equal(d$orientation, "runs_in_rows")
  expect_equal(d$components, c("MgCl2", "Kcl", "PEG8000", "Betaine", "Trehalose",
                               "TCEP", "CaCl2", "Tween20"))
  expect_false(any(c("Transcript Number", "Library Yield", "Number of Genes Detected") %in% d$components))
  expect_equal(nrow(design_matrix(d)), 88)
})

test_that("transposed design is detected and reads the same components", {
  d <- read_design(fixture("ASR4_design_PEG30_transposed.csv"))
  expect_equal(d$detected_orientation, "runs_in_columns")
  expect_equal(d$components, read_design(fixture("ASR4_design_PEG24.csv"))$components)
  m <- design_matrix(d)
  expect_equal(nrow(m), 88)
  expect_equal(max(m$PEG8000), 30)
  expect_equal(unlist(m[1, -1]), c(MgCl2 = 24, Kcl = 250, PEG8000 = 0, Betaine = 2,
                                   Trehalose = 0.8, TCEP = 20, CaCl2 = 0, Tween20 = 0.1))
})

test_that("orientation can be overridden", {
  d <- read_design(fixture("ASR4_design_PEG30_transposed.csv"), orientation = "runs_in_columns")
  expect_equal(d$orientation, "runs_in_columns")
  # Forcing the wrong orientation reads numbers as names and is rejected.
  expect_error(read_design(fixture("ASR4_design_PEG24.csv"), orientation = "runs_in_columns"),
               "Duplicate column names")
  raw <- data.frame(V1 = c("A", "B"), V2 = c("1", "2"), V3 = c("3", "4"), stringsAsFactors = FALSE)
  expect_equal(read_design(raw)$components, c("A", "B"))
  flipped <- read_design(raw, orientation = "runs_in_rows")
  expect_equal(flipped$orientation, "runs_in_rows")
  expect_equal(flipped$detected_orientation, "runs_in_columns")
  expect_equal(flipped$components, c("1", "3"))
})

test_that("names are trimmed, run-id columns ignored, blank rows dropped", {
  raw <- data.frame(V1 = c("Run", "1", "2", ""), V2 = c(" NaCl ", "50", "100", ""),
                    V3 = c("Yield", "", "", ""), stringsAsFactors = FALSE)
  d <- read_design(raw)
  expect_equal(d$columns, c("Run", "NaCl", "Yield"))
  expect_equal(d$components, "NaCl")
  expect_equal(design_matrix(d)$NaCl, c(50, 100))
})

test_that("the PEG30 design gives different volumes from PEG24", {
  r <- build_plate(fixture("ASR4_design_PEG30_transposed.csv"), fixture("ASR4_stocks.csv"))
  expect_equal(max(r$volumes$PEG8000), 75)
  expect_gt(r$summary$over_volume, 18)
})

test_that("design and stock names match case-insensitively", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$component[stocks$component == "Kcl"] <- "KCl"
  r <- build_plate(fixture("ASR4_design_PEG24.csv"), stocks)
  expect_equal(r$volumes$Kcl[1], 12.5)
})

test_that("a design factor without a stock is an error", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  expect_error(build_plate(fixture("ASR4_design_PEG24.csv"), stocks[stocks$component != "TCEP", ]),
               "TCEP: design factor has no factor row")
})
