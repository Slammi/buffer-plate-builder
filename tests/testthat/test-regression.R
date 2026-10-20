# Must match the ASR4 Excel calculator.

regression_result <- function() {
  build_plate(fixture("ASR4_design_PEG24.csv"), fixture("ASR4_stocks.csv"),
              settings = list(final_volume = 100, fixed_volume = 0, min_volume = 0.5,
                              overage = 0.15, dead_volume = 20, plate_format = 96, fill = "row"))
}

test_that("88 runs are placed row-wise on a 96-well plate", {
  r <- regression_result()
  well_of <- function(run) r$volumes$well[r$volumes$run == run]
  expect_equal(nrow(r$volumes), 88)
  expect_equal(well_of(1), "A1")
  expect_equal(well_of(12), "A12")
  expect_equal(well_of(13), "B1")
  expect_equal(well_of(88), "H4")
  expect_true(all(r$volumes$plate == 1))
})

test_that("status counts and worst case match", {
  r <- regression_result()
  expect_equal(r$summary$ok, 70)
  expect_equal(r$summary$over_volume, 18)
  expect_equal(r$summary$below_min, 0)
  expect_equal(round(100 * r$summary$worst_fraction, 1), 122.4)
})

test_that("Run 1 (A1) volumes match", {
  r <- regression_result()
  a1 <- r$volumes[r$volumes$well == "A1", ]
  expected <- c(MgCl2 = 2.4, Kcl = 12.5, PEG8000 = 0, Betaine = 2, Trehalose = 2,
                TCEP = 4, CaCl2 = 0, Tween20 = 1, `Tris-HCl pH 8.3` = 25, water = 51.1)
  expect_equal(unlist(a1[names(expected)]), expected, tolerance = 1e-9)
})

test_that("total dispensed volumes match", {
  r <- regression_result()
  total <- setNames(r$stock_prep$total_dispensed_uL, r$stock_prep$source)
  expect_equal(total[["Kcl"]], 1100)
  expect_equal(total[["PEG8000"]], 2797.5)
  expect_equal(total[["Tris-HCl pH 8.3"]], 2200)
  expect_equal(total[["Water"]], 2303.9, tolerance = 1e-9)
})

test_that("stock prep adds overage and dead volume", {
  r <- regression_result()
  kcl <- r$stock_prep[r$stock_prep$source == "Kcl", ]
  expect_equal(kcl$total_to_prepare_uL, 1100 * 1.15 + 20)
})
