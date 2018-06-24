library(testthat)
library(grattan)
library(data.table)
if (requireNamespace("taxstats", quietly = TRUE)){
  library(taxstats)
  get_sample_files_all(assign.env = .GlobalEnv)
  sample_files_all <- sample_files_all
}
library(dplyr)
library(dtplyr)
library(magrittr)
library(survey)
library(zoo)

test_check("grattan")
