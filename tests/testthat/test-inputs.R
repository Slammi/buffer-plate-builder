# Bad or unusual entries in settings, stock tables and design CSVs.

asr4_design <- function() fixture("ASR4_design_PEG24.csv")
asr4_stocks <- function() read_stocks(fixture("ASR4_stocks.csv"))

# Stock table with one value replaced by text, as if typed into a CSV.
stocks_with <- function(comp, col, value) {
  s <- asr4_stocks()
  s[[col]] <- as.character(s[[col]])
  s[[col]][s$component == comp] <- value
  normalise_stocks(s)
}

# Design CSV with one cell replaced; returns the path.
design_with <- function(col, run, value) {
  d <- utils::read.csv(asr4_design(), check.names = FALSE, colClasses = "character")
  d[[col]][run] <- value
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE, na = "")
  f
}

# ---- Settings ---------------------------------------------------------------

test_that("cleared or impossible settings are rejected with a plain message", {
  expect_error(resolve_settings(list(min_volume = NA)), "Minimum pipettable volume .* at least 0")
  expect_error(resolve_settings(list(min_volume = -1)), "Minimum pipettable volume")
  expect_error(resolve_settings(list(final_volume = NA)), "Final reaction volume .* above 0")
  expect_error(resolve_settings(list(final_volume = 0)), "Final reaction volume")
  expect_error(resolve_settings(list(fixed_volume = NA)), "Fixed volume per well")
  expect_error(resolve_settings(list(overage = NA)), "Stock overage")
  expect_error(resolve_settings(list(dead_volume = -5)), "Dead volume")
  expect_error(resolve_settings(list(replicates = NA)), "Replicates per run")
  expect_error(resolve_settings(list(fixed_volume = 100, final_volume = 100)), "less than the final")
  expect_error(resolve_settings(list(randomize = TRUE, seed = NA)), "Seed must be a number")
  expect_equal(resolve_settings(list(min_volume = 0))$min_volume, 0)
})

test_that("a cleared minimum volume stops the calculation instead of crashing it", {
  stocks <- asr4_stocks()
  dmat <- design_matrix(read_design(asr4_design()))
  expect_error(build_plate(asr4_design(), stocks, settings = list(min_volume = NA)),
               "Minimum pipettable volume")
  expect_error(factor_checks(dmat, stocks, list(min_volume = NA)), "Minimum pipettable volume")
})

# ---- Stock table --------------------------------------------------------------

test_that("text in a stock column is named, not reported as a number problem", {
  for (v in c("abc", "40 mM", "40%")) {
    expect_error(build_plate(asr4_design(), stocks_with("PEG8000", "stock_conc", v)),
                 sprintf("PEG8000: stock conc. '%s' is not a number", v), fixed = TRUE)
  }
  expect_error(build_plate(asr4_design(), stocks_with("PEG8000", "stock_conc", "")),
               "PEG8000: stock conc. is missing")
  expect_error(build_plate(asr4_design(), stocks_with("PEG8000", "stock_conc", "-40")),
               "PEG8000: stock conc. must be above 0")
})

test_that("thousands separators are read as numbers", {
  expect_equal(parse_num(c("1,000", "12,500.5", "1000", " 2,000 ")), c(1000, 12500.5, 1000, 2000))
  expect_true(is.na(parse_num("1,00")))      # not a thousands separator
  expect_true(is.na(parse_num("1.000,5")))   # European decimal comma is not accepted
  r <- build_plate(asr4_design(), stocks_with("MgCl2", "stock_conc", "1,000"))
  expect_equal(r$summary$over_volume, 18)
})

test_that("working and max stocks reject text and values of 0 or below", {
  expect_error(build_plate(asr4_design(), stocks_with("Betaine", "working_stock_conc", "abc")),
               "Betaine: working stock 'abc' is not a number", fixed = TRUE)
  expect_error(build_plate(asr4_design(), stocks_with("Betaine", "working_stock_conc", "-5")),
               "Betaine: working stock must be above 0")
  expect_error(build_plate(asr4_design(), stocks_with("PEG8000", "max_stock_conc", "abc")),
               "PEG8000: max stock 'abc' is not a number", fixed = TRUE)
  expect_error(build_plate(asr4_design(), stocks_with("PEG8000", "max_stock_conc", "-1")),
               "PEG8000: max stock must be above 0")
})

test_that("a stock above its max is a visible warning, not an error", {
  r <- build_plate(asr4_design(), stocks_with("PEG8000", "max_stock_conc", "30"))
  w <- r$problems[r$problems$severity == "warning", ]
  expect_equal(w$component, "PEG8000")
  expect_match(w$problem, "above its max stock")
})

test_that("constant final conc. problems are reported precisely", {
  tris <- "Tris-HCl pH 8.3"
  expect_error(build_plate(asr4_design(), stocks_with(tris, "final_conc", "abc")),
               "final conc. 'abc' is not a number", fixed = TRUE)
  expect_error(build_plate(asr4_design(), stocks_with(tris, "final_conc", "")), "needs a final conc.")
  expect_error(build_plate(asr4_design(), stocks_with(tris, "final_conc", "-250")), "cannot be negative")
  cc <- constant_checks(stocks_with(tris, "final_conc", "-250"), list())
  expect_equal(cc$status, "bad")
})

test_that("Inputs-page checks flag a working stock of 0 or below", {
  dmat <- design_matrix(read_design(asr4_design()))
  s <- asr4_stocks()
  s$working_stock_conc[s$component == "Betaine"] <- -5
  f <- factor_checks(dmat, s, list())
  expect_equal(f$check[f$component == "Betaine"], "Working stock must be above 0")
})

# ---- Design CSV -----------------------------------------------------------------

test_that("one bad cell keeps the column as a component and names the cell", {
  for (v in c("24 %", "abc")) {
    d <- read_design(design_with("PEG8000", 1, v))
    expect_true("PEG8000" %in% d$components)
    p <- design_problems(d)
    expect_equal(p[p$problem == "not a number", c("column", "run", "value")],
                 data.frame(column = "PEG8000", run = 1L, value = v))
    expect_error(build_plate(d, asr4_stocks()), sprintf("PEG8000 run 1 '%s' (not a number)", v), fixed = TRUE)
  }
})

test_that("thousands separators in the design are numbers", {
  d <- read_design(design_with("MgCl2", 1, "1,000"))
  expect_true("MgCl2" %in% d$components)
  expect_equal(design_matrix(d)$MgCl2[1], 1000)
})

test_that("negative and blank design cells are reported", {
  d <- read_design(design_with("PEG8000", 1, "-5"))
  expect_error(design_matrix(d), "PEG8000 run 1 '-5' (negative)", fixed = TRUE)

  d <- read_design(design_with("PEG8000", 2, ""))
  p <- design_problems(d)
  expect_equal(p$problem, "blank")
  expect_warning(m <- design_matrix(d), "treated as 0")
  expect_equal(m$PEG8000[2], 0)
})

test_that("mostly-text columns are still treated as responses", {
  raw <- data.frame(V1 = c("NaCl", "50", "100", "150"), V2 = c("Notes", "ok", "n/a", "12"),
                    stringsAsFactors = FALSE)
  expect_equal(read_design(raw)$components, "NaCl")
})
