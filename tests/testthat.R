
all.pkgs <- c("testthat", "dplyr", "dtplyr", "data.table", "magrittr", "survey", "zoo")


if (all(vapply(all.pkgs, requireNamespace, logical(1L), quietly = TRUE))) {
  
  library(testthat)
  library(grattan)
  library(hutils)
  library(data.table)
  
  library(magrittr)
  
  test_check("grattan")
}
