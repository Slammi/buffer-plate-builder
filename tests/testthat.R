# Run with: Rscript tests/testthat.R   (from the project root)
library(testthat)
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
