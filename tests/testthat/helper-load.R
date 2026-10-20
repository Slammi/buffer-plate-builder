# Source the core functions (no Shiny) and locate the fixtures.
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)

fixture <- function(name) file.path(root, "tests", "fixtures", name)
