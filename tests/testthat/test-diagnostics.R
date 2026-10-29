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
  expect_match(o$message, "^Not feasible")
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
  # Components left without room. Betaine and CaCl2 (1000 mM) are not listed:
  # their smallest level is already at the minimum volume, so they cannot be
  # made more concentrated either.
  expect_true(all(c("MgCl2", "Kcl", "Trehalose", "TCEP", "Tween20", "Tris-HCl") %in% o$infinite_components))
  expect_false(any(c("PEG8000", "Betaine", "CaCl2") %in% o$infinite_components))
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

test_that("stock_problems names the stock behind over-volume runs and its fix", {
  p <- stock_problems(asr4())
  expect_equal(p$source[1], "PEG8000")
  peg <- p[p$source == "PEG8000", ]
  expect_equal(peg$status, "bad")
  expect_equal(peg$largest_in_over_volume, 18)
  expect_equal(peg$over_volume_runs, 18)
  expect_match(peg$problem, "at least 63.9 % v/v makes every well fit")
  expect_true(all(p$status[p$source != "PEG8000"] == "ok"))
  expect_equal(tail(p$source, 1), "Water")
})

test_that("stock_problems suggests a working stock for below-min levels", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  stocks$stock_conc[stocks$component == "Betaine"] <- 1000   # 1 mM -> 0.1 µL, 2 mM -> 0.2 µL
  r <- asr4(stocks)
  p <- stock_problems(r)
  bet <- p[p$source == "Betaine", ]
  expect_equal(bet$status, "warn")
  expect_equal(bet$runs_below_min, sum(r$design_matrix$Betaine > 0))
  expect_equal(bet$smallest_volume_uL, 0.1)
  expect_match(bet$problem, "working stock of at most 200 mM")   # 1 mM * 100 µL / 0.5 µL

  # With that working stock the problem is gone.
  stocks$working_stock_conc[stocks$component == "Betaine"] <- 200
  p2 <- stock_problems(asr4(stocks))
  expect_true(all(p2$status[startsWith(p2$source, "Betaine")] == "ok"))
  expect_true("Betaine (working)" %in% p2$source)
})

test_that("stock_problems flags water below the minimum", {
  stocks <- read_stocks(fixture("ASR4_stocks.csv"))
  # Run 43 (worst) has 62.4 µL of other components and 24% PEG: a 64.35% PEG
  # stock leaves ~0.3 µL of water.
  stocks$stock_conc[stocks$component == "PEG8000"] <- 64.35
  r <- asr4(stocks)
  w <- stock_problems(r)
  w <- w[w$source == "Water", ]
  expect_equal(w$runs_below_min, sum(r$runs$water > 0 & r$runs$water < 0.5))
  expect_gt(w$runs_below_min, 0)
  expect_equal(w$status, "warn")
})

test_that("the optimiser counts the volume taken from working stocks", {
  # A = 1 uses the working stock (10 uL, not 0.1 uL from the main stock), so
  # the run A = 1, B = 95 overflows (105 uL) although main stocks alone fit.
  raw <- data.frame(V1 = c("A", "0", "1", "1"), V2 = c("B", "95", "95", "0"), stringsAsFactors = FALSE)
  stocks <- data.frame(component = c("A", "B"), role = "factor", stock_conc = c(1000, 100),
                       working_stock_conc = c(10, NA))
  r <- build_plate(read_design(raw), stocks)
  expect_equal(r$summary$over_volume, 1)
  o <- optimise_stocks(r)
  expect_true(o$feasible)
  b <- o$stocks$suggested_rounded[o$stocks$component == "B"]
  expect_gte(b, 95 / 0.9)                            # 95 uL must fit in the 90 uL left
  stocks$stock_conc[2] <- b
  expect_equal(build_plate(read_design(raw), stocks)$summary$over_volume, 0)
  expect_equal(o$worst_fraction, max(95 / b + 0.1, 95 / b), tolerance = 1e-9)
})

test_that("the optimiser does not push a pipettable level below the minimum", {
  # B's smallest level (5) needs at least 0.5 uL: stock at most 5 * 100 / 0.5 = 1000.
  raw <- data.frame(V1 = c("A", "60", "0"), V2 = c("B", "50", "5"), stringsAsFactors = FALSE)
  stocks <- data.frame(component = c("A", "B"), role = "factor", stock_conc = c(100, 100),
                       max_stock_conc = c(100, NA))
  r <- build_plate(read_design(raw), stocks)
  o <- optimise_stocks(r, hold = "A")
  expect_true(o$feasible)
  b <- o$stocks$suggested_rounded[o$stocks$component == "B"]
  expect_lte(b, 1000)
  stocks$stock_conc[2] <- b
  r2 <- build_plate(read_design(raw), stocks)
  expect_equal(r2$summary$over_volume, 0)
  expect_equal(r2$summary$below_min, 0)
})

test_that("held components are not changed by the optimiser", {
  o <- optimise_stocks(asr4(), hold = "PEG8000")
  expect_true(o$feasible)
  expect_equal(o$stocks$suggested_stock[o$stocks$component == "PEG8000"], 40)
  expect_true(any(o$stocks$changed))
})
