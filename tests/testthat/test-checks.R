# Inputs-page checks, compared with the Inputs tab of the Excel calculator.

asr4_checks <- function(stocks = read_stocks(fixture("ASR4_stocks.csv")), settings = list()) {
  dmat <- design_matrix(read_design(fixture("ASR4_design_PEG24.csv")))
  list(f = factor_checks(dmat, stocks, settings), c = constant_checks(stocks, settings))
}

test_that("factor rows give range, volume at max and at smallest level", {
  f <- asr4_checks()$f
  mg <- f[f$component == "MgCl2", ]
  expect_equal(c(mg$design_min, mg$design_max, mg$vol_at_max, mg$fraction_at_max,
                 mg$smallest_level, mg$vol_at_smallest), c(0, 24, 2.4, 0.024, 12, 1.2))
  expect_equal(mg$check, "OK")
  kcl <- f[f$component == "Kcl", ]
  expect_equal(c(kcl$vol_at_max, kcl$vol_at_smallest), c(25, 12.5))
})

test_that("a stock using more than half the well at max is flagged", {
  f <- asr4_checks()$f
  peg <- f[f$component == "PEG8000", ]
  expect_equal(peg$vol_at_max, 60)
  expect_equal(peg$check, "Uses >50% of well at max")
  expect_equal(peg$status, "warn")
})

test_that("levels below the minimum point to a working stock", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$stock_conc[stocks$component == "TCEP"] <- 5000
  f <- asr4_checks(stocks)$f
  expect_equal(f$check[f$component == "TCEP"], "Below min - use a diluted working stock")

  stocks$working_stock_conc[stocks$component == "TCEP"] <- 500
  f <- asr4_checks(stocks)$f
  expect_equal(f$check[f$component == "TCEP"], "OK (low levels from working stock)")
  expect_equal(f$status[f$component == "TCEP"], "ok")
})

test_that("missing stock and impossible stock are reported", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$stock_conc[stocks$component == "Betaine"] <- NA
  stocks$stock_conc[stocks$component == "PEG8000"] <- 20
  f <- asr4_checks(stocks)$f
  expect_equal(f$check[f$component == "Betaine"], "Enter stock conc.")
  expect_equal(f$status[f$component == "PEG8000"], "bad")
})

test_that("constant rows match the Excel layout", {
  cc <- asr4_checks()$c
  expect_equal(cc$component, "Tris-HCl pH 8.3")
  expect_equal(c(cc$vol_per_well, cc$fraction_of_well), c(25, 0.25))
  expect_equal(cc$check, "OK")

  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$final_conc[stocks$role == "constant"] <- 2000
  expect_equal(asr4_checks(stocks)$c$check, "Final conc. exceeds stock")
})
