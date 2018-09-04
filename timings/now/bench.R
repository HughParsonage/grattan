
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
  invisible(gc(0,1,1))
  as.data.frame(mm) %>%
    as.data.table %>%
    .[, Date := as.character(Sys.Date())] %T>%
    fwrite(paste0("timings/now/result-", (i <<- i + 1L), ".tsv"), sep = "\t", append = FALSE, eol = "\n") %>%
    fwrite(paste0("timings/now/result-", (i), "-dates.tsv"), sep = "\t", append = TRUE, eol = "\n")
}
invisible(gc(0,1,1))
microbenchmark(
  cpi_inflator(from_fy = "2014-15", to_fy = "2015-16"),
  wage_inflator(from_fy = "2014-15", to_fy = "2015-16"),
  lf_inflator_fy(from_fy = "2014-15", to_fy = "2015-16"),
  times = 1L, control = list(order = "inorder")) %>%
  save_it

the_next_fy <- next_fy(date2fy(Sys.Date()), h = 2L)

microbenchmark(
  cpi_inflator(from_fy = "2014-15", to_fy = the_next_fy),
  wage_inflator(from_fy = "2014-15", to_fy = the_next_fy),
  lf_inflator_fy(from_fy = "2014-15", to_fy = the_next_fy),
  times = 1L, control = list(order = "inorder")) %>%
  save_it


from_fys50K <- sample(yr2fy(2003:2016), size = 50e3, replace = TRUE)


microbenchmark(
  cpi_inflator(from_fy = from_fys50K, to_fy = "2015-16"),
  wage_inflator(from_fy = from_fys50K, to_fy = "2015-16"),
  lf_inflator_fy(from_fy = from_fys50K, to_fy = "2015-16"),
  times = 1L, control = list(order = "inorder")) %>%
  save_it



microbenchmark(
  sample_file_1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = .SD)],
  times = 1L, control = list(order = "inorder")) %>%
  save_it

microbenchmark(
  project(sample_file_1314, h = 2L),
  times = 1L, control = list(order = "inorder")) %>%
  save_it

s1314 <- copy(sample_file_1314)
sfall <- get_sample_files_all()
microbenchmark(
  s1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = .SD)],
  sfall[, income_tax(Taxable_Income, fy.year, .dots.ATO = .SD)],
  times = 1L, control = list(order = "inorder")) %>%
  save_it

x2014L_10K <- rep_len(2014L, 10e3)
x1984_2015_20M <- rep(1991:2010, each = 1e6, length.out = 20e6)
microbenchmark(
  yr2fy(2014L),
  yr2fy(x2014L_10K),
  yr2fy(x1984_2015_20M),
  times = 2L, control = list(order = "inorder")) %>%
  save_it

microbenchmark(
  model_income_tax(s1314, "2015-16"),
  model_thresholds = model_income_tax(s1314, "2015-16", ordinary_tax_thresholds = c(0, 20e3, 40e3, 80e3, 180e3)),
  model_medicare = model_income_tax(s1314, "2015-16", medicare_levy_lower_sapto_threshold = 30e3, medicare_levy_upper_sapto_threshold = 37500),
  model_cgt = model_income_tax(s1314, "2015-16", cgt_discount_rate = 0.0),
  model_lito = model_income_tax(s1314, "2015-16", lito_max_offset = 500),
  times = 2L, control = list(order = "inorder")) %>%
  save_it

dt <- data.table(x = seq(0, 1e4, length.out = 1e6),
                 y = rnorm(1e6),
                 wt = runif(1e6, 10, 150))

microbenchmark(
  dt[, weighted_ntile(y, w = wt, n = 10)],
  dt[, weighted_ntile(x, w = wt, n = 10)],
  times = 2L, control = list(order = "inorder")) %>%
  save_it

shell("git add timings/now/result*tsv")



