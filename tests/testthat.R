
all.pkgs <- c("testthat", "dplyr", "dtplyr", "data.table", "magrittr", "survey", "zoo")


if (all(vapply(all.pkgs, requireNamespace, logical(1), quietly = TRUE))) {
  
  library(testthat)
  library(grattan)
  library(dplyr)
  library(dtplyr)
  
  library(data.table)
  if (requireNamespace("taxstats", quietly = TRUE) &&
      packageVersion("taxstats") >= package_version("0.0.5")) {
    library(taxstats)
  }
  
  library(magrittr)
  library(survey)
  library(zoo)
  library(hutils)
  library(RcppParallel)
  setThreadOptions(numThreads = 2)
  
  test_check("grattan")
}
