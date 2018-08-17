
library(microbenchmark)
library(magrittr)
library(hutils)
library(data.table)
library(taxstats)

attach(asNamespace("grattan"))

stopifnot(file.exists("DESCRIPTION"))
options("digits" = 2)
options("scipen" = 99)

i = 0L
save_it <- function(mm) {
  as.data.frame(mm) %>%
    as.data.table %>%
    .[, Date := as.character(Sys.Date())] %>%
    fwrite(paste0("timings/now/result-", (i <<- i + 1L), ".tsv"), sep = "\t", append = TRUE)
}

microbenchmark(
  cpi_inflator(from_fy = "2014-15", to_fy = "2015-16"),
  wage_inflator(from_fy = "2014-15", to_fy = "2015-16"),
  lf_inflator_fy(from_fy = "2014-15", to_fy = "2015-16"),
  times = 1L) %>%
  save_it

the_next_fy <- next_fy(date2fy(Sys.Date()), h = 2L)

microbenchmark(
  cpi_inflator(from_fy = "2014-15", to_fy = the_next_fy),
  wage_inflator(from_fy = "2014-15", to_fy = the_next_fy),
  lf_inflator_fy(from_fy = "2014-15", to_fy = the_next_fy),
  times = 1L) %>%
  save_it


from_fys50K <- sample(yr2fy(2003:2016), size = 50e3, replace = TRUE)


microbenchmark(
  cpi_inflator(from_fy = from_fys50K, to_fy = "2015-16"),
  wage_inflator(from_fy = from_fys50K, to_fy = "2015-16"),
  lf_inflator_fy(from_fy = from_fys50K, to_fy = "2015-16"),
  times = 1L) %>%
  save_it



microbenchmark(
  sample_file_1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = .SD)],
  times = 1L) %>%
  save_it

microbenchmark(
  project(sample_file_1314, h = 2L),
  times = 1L) %>%
  save_it




