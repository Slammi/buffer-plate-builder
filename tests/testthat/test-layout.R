test_that("column-wise fill goes down the columns", {
  l <- layout_runs(10, list(fill = "column"))
  expect_equal(l$well[1:3], c("A1", "B1", "C1"))
  expect_equal(l$well[9], "A2")
})

test_that("384-well plates have 16 x 24 wells", {
  w <- plate_wells(384)
  expect_equal(nrow(w), 384)
  expect_equal(tail(w$well, 1), "P24")
  expect_equal(layout_runs(25, list(plate_format = 384))$well[25], "B1")
})

test_that("edge exclusion and reserved wells are skipped", {
  w <- plate_wells(96, exclude_edges = TRUE)
  expect_equal(sum(w$available), 60)
  l <- layout_runs(3, list(exclude_edges = TRUE, reserved_wells = "b03, B4"))
  expect_equal(l$well, c("B2", "B5", "B6"))
})

test_that("runs spill onto further plates", {
  l <- layout_runs(100, list())
  expect_equal(max(l$plate), 2)
  expect_equal(l$well[l$run == 97], "A1")
  expect_equal(l$plate[l$run == 97], 2)
})

test_that("replicates sit next to each other", {
  l <- layout_runs(3, list(replicates = 2))
  expect_equal(l$run, c(1, 1, 2, 2, 3, 3))
  expect_equal(l$replicate, c(1, 2, 1, 2, 1, 2))
})

test_that("randomization is reproducible and leaves the global RNG alone", {
  set.seed(42); before <- runif(1); set.seed(42)
  a <- layout_runs(20, list(randomize = TRUE, seed = 7))
  expect_equal(runif(1), before)
  b <- layout_runs(20, list(randomize = TRUE, seed = 7))
  expect_equal(a, b)
  expect_false(identical(a$run, 1:20))
  expect_setequal(a$run, 1:20)
})

test_that("bad settings are rejected", {
  expect_error(resolve_settings(list(plate_format = 48)), "96 or 384")
  expect_error(resolve_settings(list(volume = 1)), "Unknown settings")
  expect_error(normalise_wells("Z99"), "Not well names")
})
