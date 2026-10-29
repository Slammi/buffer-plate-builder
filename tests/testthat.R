# Run with: Rscript tests/testthat.R   (from the project root; about 3 minutes)
# Quick run (20 instead of 120 random designs):
#   Rscript tests/testthat.R quick
if ("quick" %in% commandArgs(trailingOnly = TRUE)) Sys.setenv(QUICK_TESTS = "1")
library(testthat)
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
