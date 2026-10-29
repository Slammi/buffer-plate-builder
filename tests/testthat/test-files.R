# Real-world file quirks in design and stock CSVs.

write_lines <- function(lines, eol = "\n", bom = FALSE) {
  f <- tempfile(fileext = ".csv")
  con <- file(f, open = "wb")
  if (bom) writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = eol), eol))), con)
  close(con)
  f
}

small_design <- c("NaCl,Glycerol,Yield", "50,5,", "100,10,", "0,0,")
small_stocks <- c("component,role,units,stock_conc,final_conc", "NaCl,factor,mM,1000,",
                  "Glycerol,factor,%,50,", "Tris,constant,mM,1000,20")

expect_small_result <- function(design_file, stocks_file) {
  r <- build_plate(design_file, stocks_file)
  expect_equal(r$design$components, c("NaCl", "Glycerol"))
  expect_equal(r$volumes$NaCl, c(5, 10, 0))
  expect_equal(r$volumes$Glycerol, c(10, 20, 0))
  expect_equal(r$volumes$Tris, c(2, 2, 2))
  invisible(r)
}

test_that("the plain files work (baseline)", {
  expect_small_result(write_lines(small_design), write_lines(small_stocks))
})

test_that("Windows line endings and a byte-order mark are handled", {
  expect_small_result(write_lines(small_design, "\r\n", bom = TRUE),
                      write_lines(small_stocks, "\r\n", bom = TRUE))
})

test_that("semicolon-separated files with decimal commas are read (European Excel)", {
  design <- c("NaCl;Glycerol;Yield", "50;5;", "100;10;", "0;0;")
  stocks <- c("component;role;units;stock_conc;final_conc", "NaCl;factor;mM;1000;",
              "Glycerol;factor;%;50;", "Tris;constant;mM;1000;20")
  expect_small_result(write_lines(design), write_lines(stocks))

  design_dec <- c("NaCl;Glycerol", "50;2,5", "100;10", "0;0")
  r <- build_plate(write_lines(design_dec), write_lines(stocks))
  expect_equal(r$volumes$Glycerol[1], 5)
})

test_that("blank rows and columns, quoted fields and spaces are ignored", {
  design <- c('"NaCl"," Glycerol ","Yield",,', '50,5,,,', '', '100,10,,,', '0,0,,,', ',,,,', '')
  stocks <- c(small_stocks[1], '"NaCl","factor","mM","1000",""', small_stocks[3:4], ",,,,")
  expect_small_result(write_lines(design), write_lines(stocks))
})

test_that("µ, % and other characters in names and units survive", {
  design <- c("NaCl (µM),PEG 8000 %,Yield", "50,5,", "100,10,", "0,0,")
  stocks <- c("component,role,units,stock_conc,final_conc", "NaCl (µM),factor,µM,1000,",
              "PEG 8000 %,factor,% w/v,50,", "Tris,constant,mM,1000,20")
  r <- build_plate(write_lines(design), write_lines(stocks))
  expect_equal(r$design$components, c("NaCl (µM)", "PEG 8000 %"))
  expect_equal(r$volumes[["PEG 8000 %"]], c(10, 20, 0))
  expect_true("PEG 8000 % - 50 % w/v" %in% dragonfly_dfd(r))
})

test_that("stock columns may come in any order and case, with extra columns", {
  stocks <- c("Stock_Conc,Component,Notes,Units,ROLE,Final_Conc,Supplier",
              "1000,NaCl,,mM,factor,,Sigma", "50,Glycerol,,%,factor,,Fisher", "1000,Tris,,mM,constant,20,NEB")
  expect_small_result(write_lines(small_design), write_lines(stocks))
})

test_that("names that differ only in case are rejected clearly", {
  expect_error(read_design(write_lines(c("KCl,Kcl", "1,2"))), "Duplicate column names")
  stocks <- c(small_stocks, "nacl,factor,mM,500,")
  expect_error(build_plate(write_lines(small_design), write_lines(stocks)), "listed more than once")
})

test_that("component names used by the app's own columns are rejected clearly", {
  for (nm in c("Water", "plate", "well", "status", "fixed", "total", "Diluent")) {
    design <- c(paste0("NaCl,", nm), "50,5", "100,10")
    stocks <- c(small_stocks[1:2], paste0(nm, ",factor,mM,100,"))
    expect_error(build_plate(write_lines(design), write_lines(stocks)),
                 sprintf("'%s' .* used by the app", nm), info = nm)
  }
})

test_that("a design with only response columns says so", {
  expect_error(build_plate(write_lines(c("Yield,Notes", ",", ",")), write_lines(small_stocks)),
               "No component columns")
})
