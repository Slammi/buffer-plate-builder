asr4 <- function(stocks = read_stocks(fixture("ASR4_stocks.csv")), ...) {
  build_plate(fixture("ASR4_design_PEG24.csv"), stocks, ...)
}

test_that("failing wells report overflow and the largest component", {
  r <- asr4()
  expect_equal(nrow(r$failing), 18)
  worst <- r$failing[1, ]
  expect_equal(worst$overflow_uL, 22.4, tolerance = 1e-9)
  expect_equal(worst$largest_component, "PEG8000")
})

test_that("headroom is negative when wells fail", {
  r <- asr4()
  expect_equal(r$summary$headroom_uL, -22.4, tolerance = 1e-9)
})

test_that("minimum PEG stock makes every well fit", {
  r <- asr4()
  ms <- r$min_stocks
  peg <- ms$min_stock_needed[ms$component == "PEG8000"]
  expect_false(ms$fits_now[ms$component == "PEG8000"])
  # Components never present in an overflowing well cannot fix it.
  expect_true(is.infinite(ms$min_stock_needed[ms$component == "Betaine"]))

  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$stock_conc[stocks$component == "PEG8000"] <- peg
  fixed <- asr4(stocks)
  expect_equal(fixed$summary$over_volume, 0)
  expect_equal(fixed$summary$worst_fraction, 1, tolerance = 1e-9)
})

test_that("minimum stock above max_stock_conc is flagged", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$max_stock_conc[stocks$component == "PEG8000"] <- 50
  ms <- asr4(stocks)$min_stocks
  expect_true(ms$exceeds_max[ms$component == "PEG8000"])
})

test_that("optimiser finds stocks within limits that fit every well", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$max_stock_conc <- stocks$stock_conc * 2
  stocks$max_stock_conc[stocks$component == "PEG8000"] <- 50
  r <- asr4(stocks)
  o <- optimise_stocks(r)
  expect_true(o$feasible)
  expect_true(all(o$stocks$suggested_rounded <= o$stocks$max_stock_conc + 1e-9))
  expect_true(all(o$stocks$suggested_rounded >= o$stocks$stock_conc))

  stocks2 <- stocks
  stocks2$stock_conc <- o$stocks$suggested_rounded[match(stocks2$component, o$stocks$component)]
  expect_equal(asr4(stocks2)$summary$over_volume, 0)
})

test_that("optimiser reports infeasible designs", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$max_stock_conc <- stocks$stock_conc
  o <- optimise_stocks(asr4(stocks))
  expect_false(o$feasible)
  expect_match(o$message, "No set of stocks")
})

test_that("a held stock that fills whole wells is infeasible, not infinite stocks", {
  # PEG stock 30% with a 30% design maximum: PEG alone takes 100% of those wells.
  stocks <- data.frame(component = c("MgCl2", "Kcl", "PEG8000", "Betaine", "Trehalose", "TCEP",
                                     "CaCl2", "Tween20", "Tris-HCl"),
                       role = c(rep("factor", 8), "constant"),
                       stock_conc = c(1000, 1000, 30, 1000, 40, 1000, 1000, 10, 1000),
                       final_conc = c(rep(NA, 8), 38))
  r <- build_plate(fixture("ASR4_design_PEG30_transposed.csv"), stocks, settings = list(min_volume = 0.2))
  o <- optimise_stocks(r, hold = "PEG8000")
  expect_false(o$feasible)
  expect_match(o$message, "PEG8000 is held at 30")
  expect_match(o$message, "100% of the well")
  expect_setequal(o$infinite_components, setdiff(stocks$component, "PEG8000"))
  expect_true(all(is.na(o$stocks$suggested_rounded)))

  # Without the hold, raising the PEG stock is a real solution.
  free <- optimise_stocks(r)
  expect_true(free$feasible)
  expect_true(all(is.finite(free$stocks$suggested_rounded)))
  expect_gt(free$stocks$suggested_rounded[free$stocks$component == "PEG8000"], 30)
})

test_that("a stock at its max that fills whole wells is explained", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$stock_conc[stocks$component == "PEG8000"] <- 24
  stocks$max_stock_conc[stocks$component == "PEG8000"] <- 24
  o <- optimise_stocks(asr4(stocks))
  expect_false(o$feasible)
  expect_match(o$message, "PEG8000 is at its max stock")
})

test_that("held components are not changed by the optimiser", {
  o <- optimise_stocks(asr4(), hold = "PEG8000")
  expect_true(o$feasible)
  expect_equal(o$stocks$suggested_stock[o$stocks$component == "PEG8000"], 40)
  expect_true(any(o$stocks$changed))
})
