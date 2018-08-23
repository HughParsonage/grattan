
all.pkgs <- c("testthat", "dplyr", "dtplyr", "data.table", "magrittr", "survey", "zoo")


if (all(vapply(all.pkgs, requireNamespace, logical(1L), quietly = TRUE))) {
  
  library(testthat)
  library(grattan)
  library(dplyr)
  library(dtplyr)
  library(hutils)
  library(data.table)
  
  if (requireNamespace("taxstats", quietly = TRUE)) {
    
    library(taxstats)
    sample_files_all <-
      rbindlist(lapply(list(`2003-04` = sample_file_0304,
                            # `2004-05` = sample_file_0405, `2005-06` = sample_file_0506,
                            `2006-07` = sample_file_0607, #`2007-08` = sample_file_0708,
                            `2008-09` = sample_file_0809, `2009-10` = sample_file_0910,
                            `2010-11` = sample_file_1011, `2011-12` = sample_file_1112,
                            `2012-13` = sample_file_1213, `2013-14` = sample_file_1314),
                       data.table::setDT),
                use.names = TRUE,
                fill = TRUE,
                idcol = "fy.year")
    sample_files_all[, WEIGHT := hutils::if_else(fy.year > '2010-11', 50L, 100L)]
  }
  
  library(magrittr)
  library(survey)
  library(zoo)
  
  test_check("grattan")
}
