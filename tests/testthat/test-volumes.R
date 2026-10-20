small_design <- function(...) {
  cols <- list(...)
  raw <- rbind(names(cols), do.call(cbind, lapply(cols, as.character)))
  read_design(as.data.frame(raw, stringsAsFactors = FALSE))
}

test_that("below-minimum volumes are flagged, including water", {
  d <- small_design(NaCl = c(0, 0.2, 100), Glycerol = c(0, 0, 44.8))
  stocks <- data.frame(component = c("NaCl", "Glycerol"), role = "factor", units = "mM",
                       stock_conc = c(1000, 50))
  r <- build_plate(d, stocks)
  expect_equal(r$volumes$status, c("OK", "BELOW MIN", "BELOW MIN"))
  expect_equal(r$runs$below_min_sources, c("", "NaCl", "Water"))
})

test_that("working stock is used for levels below the minimum", {
  d <- small_design(NaCl = c(0.2, 100))
  stocks <- data.frame(component = "NaCl", role = "factor", units = "mM",
                       stock_conc = 1000, working_stock_conc = 10)
  r <- build_plate(d, stocks)
  expect_equal(r$volumes$status, c("OK", "OK"))
  expect_equal(r$volumes$`NaCl (working)`, c(2, 0))
  expect_equal(r$volumes$NaCl, c(0, 10))
  prep <- r$stock_prep
  expect_equal(prep$stock_conc[prep$source == "NaCl (working)"], 10)
  expect_equal(prep$wells[prep$source == "NaCl (working)"], 1)
})

test_that("fixed volume reduces water and counts toward the fraction used", {
  d <- small_design(NaCl = c(100))
  stocks <- data.frame(component = "NaCl", role = "factor", stock_conc = 1000)
  r <- build_plate(d, stocks, settings = list(final_volume = 50, fixed_volume = 20))
  expect_equal(r$volumes$NaCl, 5)
  expect_equal(r$volumes$water, 25)
  expect_equal(r$volumes$fraction_used, 0.5)
  expect_true("Fixed mix" %in% r$picklist$source)
  expect_equal(tail(r$addition_order, 1), "Fixed mix")
})

test_that("feasibility does not depend on the final volume", {
  base <- build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"))
  big <- build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"),
                     settings = list(final_volume = 250))
  expect_equal(big$runs$fraction_used, base$runs$fraction_used)
  expect_equal(big$summary$over_volume, base$summary$over_volume)
})

test_that("over-volume wells report negative water and a larger total", {
  r <- build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"))
  bad <- r$volumes[r$volumes$status == "OVER VOLUME", ]
  expect_true(all(bad$water < 0))
  expect_true(all(bad$total > 100))
  ok <- r$volumes[r$volumes$status == "OK", ]
  expect_equal(ok$total, rep(100, nrow(ok)))
})
