
all.pkgs <- c("testthat", "dplyr", "dtplyr", "data.table", "magrittr", "survey", "zoo")


if (all(vapply(all.pkgs, requireNamespace, logical(1L), quietly = TRUE))) {
  
  library(testthat)
  library(grattan)
  library(hutils)
  library(data.table)
  library(magrittr)
  
  if (identical(Sys.getenv("CIRCLECI"), "true")) {
    test_check("grattan",
               reporter = JunitReporter$new(file = "junit_result.xml"))
  } else {
    test_check("grattan")
  }
}
