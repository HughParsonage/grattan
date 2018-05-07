library(testthat)
library(grattan)

library(data.table)
if (requireNamespace("taxstats", quietly = TRUE)){
  library(taxstats)
}
library(dplyr)
library(dtplyr)
library(magrittr)
library(survey)
library(zoo)

test_check("grattan")
