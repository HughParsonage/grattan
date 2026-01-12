
all.pkgs <- c("testthat", "data.table", "magrittr", "withr")


if (all(vapply(all.pkgs, requireNamespace, logical(1L), quietly = TRUE))) {
  
  library(testthat)
  library(grattan)
  library(hutils)
  library(data.table)
  library(magrittr)
  setDTthreads(1L)
  if (identical(Sys.getenv("CIRCLECI"), "true")) {
    test_check("grattan",
               reporter = JunitReporter$new(file = "junit_result.xml"))
  } else {
    test_check("grattan")
  }
}
