## ----setup, include = FALSE----------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadPackages-1------------------------------------------------------
library(mgcv)
library(lattice)
library(dtplyr)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(ggrepel)
library(viridis)
library(knitr)
library(hutils)
library(magrittr)
library(data.table)

## ----loadPackages-2------------------------------------------------------
templib <- tempfile()
hutils::provide.dir(templib)

# ## ----loadPackages-3------------------------------------------------------
# install.packages("https://raw.githubusercontent.com/hughparsonage/drat/gh-pages/src/contrib/taxstats_0.0.5.1415.tar.gz",
#                  dependencies = FALSE,
#                  quiet = FALSE,
#                  lib = templib,
#                  verbose = TRUE,
#                  repos = NULL)
# 
# ## ----loadPackages-4------------------------------------------------------
# library("taxstats",
#         lib.loc = templib,
#         verbose = TRUE,
#         character.only = TRUE)
library(taxstats)
library(data.table)

sample_file_1415 <- SampleFile1415::sample_file_1415
sample_file_1516 <- fread("~/ozTaxData/data-raw/2016_sample_file.csv", logical01 = FALSE)

## ----sample_files_all----------------------------------------------------
sample_files_all <-
    rbindlist(lapply(list(`2003-04` = sample_file_0304, 
                          `2004-05` = sample_file_0405,
                          `2005-06` = sample_file_0506, 
                          `2006-07` = sample_file_0607,
                          `2007-08` = sample_file_0708, 
                          `2008-09` = sample_file_0809,
                          `2009-10` = sample_file_0910, 
                          `2010-11` = sample_file_1011,
                          `2011-12` = sample_file_1112, 
                          `2012-13` = sample_file_1213,
                          `2013-14` = sample_file_1314,
                          `2014-15` = sample_file_1415,
                          `2015-16` = sample_file_1516), 
                     data.table::as.data.table),
              use.names = TRUE,
              fill = TRUE, 
              idcol = "fy.year")
sample_files_all[, WEIGHT := hutils::if_else(fy.year > '2010-11', 50L, 100L)]
age_range_decoder <- as.data.table(age_range_decoder)

## ----load-grattan--------------------------------------------------------
library(grattan)

## ------------------------------------------------------------------------
revenue_foregone <- function(dt) {
  out <- dt[, sum((as.integer(new_tax) - baseline_tax) * WEIGHT)]
  class(out) <- "revenue_foregone"
  out
}
print.revenue_foregone <- function(x) {
  if (x < 0) {
    pre <- paste0("\u2212", "$")
    x <- -x
  } else {
    pre <- "$"
  }
  if (x > 1e9) {
    res <- paste0(pre, prettyNum(round(x / 1e9, 2), big.mark = ","), " bn")
  } else {
    res <- paste0(pre, prettyNum(round(x / 1e6, 0), big.mark = ","), " million")
  }
  print(res)
}


## ------------------------------------------------------------------------
if (file.exists("~/ozTaxData/data-raw/2016_sample_file.csv")) {
  sample_file_1516 <- fread("~/ozTaxData/data-raw/2016_sample_file.csv")
  s1819 <- project(sample_file_1516, h = 3L, fy.year.of.sample.file = "2015-16")
} else {
  s1819 <- project(sample_file_1415_synth, h = 4L)
}

## ----lito_models---------------------------------------------------------
s1819_lito_500 <- 
  s1819 %>%
  model_income_tax(baseline_fy = "2018-19",
                   lito_max_offset = 500)
s1819_lito_400 <-
  s1819 %>%
  model_income_tax(baseline_fy = "2018-19",
                   lito_max_offset = 400)

s1819_lito_taper0.05 <- 
  s1819 %>%
  model_income_tax(baseline_fy = "2018-19",
                   lito_taper = 0.05)

s1819_lito_double_offset <- 
  s1819 %>%
  model_income_tax(baseline_fy = "2018-19",
                   lito_max_offset = 445*2)

s1819_Budget_no_lamington <- 
  s1819 %>% 
  model_income_tax(baseline_fy = "2017-18",
                   ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45),
                   ordinary_tax_thresholds = c(0, 18200, 37e3, 90e3, 180e3),
                   lito_max_offset = 445,
                   lamington = FALSE)

s1819_Budget_lamington <- 
  s1819 %>% 
  model_income_tax(baseline_fy = "2017-18",
                   ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45),
                   ordinary_tax_thresholds = c(0, 18200, 37e3, 90e3, 180e3),
                   lito_max_offset = 445,
                   lamington = TRUE)

s1819_Budget_baseline <- 
  s1819 %>% 
  model_income_tax(baseline_fy = "2017-18")

wage_forecasts <- 
  data.table(fy_year = yr2fy(2018:2028),
             r = c(2.25, 2.75, 3.25, 3.5, 3.5, rep(3.5, 6)) / 100)
lf_forecasts <- 
  data.table(fy_year = yr2fy(2018:2028),
             r = c(2.75, 1.50, 1.50, 1.50, 1.25, rep(1.25, 6)) / 100)


ordinary_tax_rates <- function(h) {
  if (h < 9) {
    c(0, 0.19, 0.325, 0.37, 0.45)
  } else {
    c(0, 0.19, 0.325, 0.45)  
  }
}

ordinary_tax_thresholds <- function(h) {
  if (h < 7) {
    c(0, 18200, 37e3, 90e3, 180e3)
  } else if (h < 9) {
    c(0, 18200, 41e3, 90e3, 180e3)
  } else {
    c(0, 18200, 41e3, 200e3)
  }
}

.do_medicare_levy <- function(x2018, fy) {
  stopifnot(length(fy) == 1L, is.fy(fy))
  if (fy == "2018-19") {
    return(x2018)
  } else {
    return(round(cpi_inflator(x2018, from_fy = "2018-19", to_fy = fy), -1))
  }
}

medicare_levy_lower_threshold <- function(fy) {
  .do_medicare_levy(21980, fy)
}

medicare_levy_lower_sapto_threshold <- function(fy) {
  .do_medicare_levy(34758, fy)
}

medicare_levy_lower_family_threshold <- function(fy) {
  .do_medicare_levy(48385, fy)
}

medicare_levy_lower_family_sapto_threshold <- function(fy) {
  .do_medicare_levy(48385, fy)
}

medicare_levy_lower_up_for_each_child <- function(fy) {
  .do_medicare_levy(3406, fy)
}

.project_to <- function(h, use.Treasury) {
  project(sample_file_1516,
          h = h,
          wage.series = if (use.Treasury) wage_forecasts,
          lf.series = if (use.Treasury) lf_forecasts)
}



.project_useTreasurys <- lapply(1:15, .project_to, use.Treasury = TRUE)
.project_useGrattans <- lapply(1:15, .project_to, use.Treasury = FALSE)


model_Budgets <- function(fy_year, use.Treasury = TRUE) { 
  h <- as.integer(fy2yr(fy_year) - 2016L)
  
  if (use.Treasury) {
    s1920 <- .project_useTreasurys[[h]]
  } else {
    s1920 <- .project_useGrattans[[h]]
  }
  
  model_income_tax <- function(...) {
    grattan::model_income_tax(
      sample_file = s1920, 
      baseline_fy = "2017-18",
      medicare_levy_lower_threshold = medicare_levy_lower_threshold(fy_year),
      medicare_levy_lower_sapto_threshold = medicare_levy_lower_sapto_threshold(fy_year),
      medicare_levy_lower_family_threshold = medicare_levy_lower_family_threshold(fy_year),
      medicare_levy_lower_family_sapto_threshold = medicare_levy_lower_family_sapto_threshold(fy_year),
      medicare_levy_lower_up_for_each_child = medicare_levy_lower_up_for_each_child(fy_year),
      warn_upper_thresholds = FALSE,
      ...) %>%
      .[, new_tax := as.integer(new_tax)] 
  }
  
  list(Budget2018_baseline = model_income_tax(),
       Budget2018_just_rates = model_income_tax(ordinary_tax_rates =  ordinary_tax_rates(h),
                                                ordinary_tax_thresholds = ordinary_tax_thresholds(h),
                                                lito_202223 = FALSE,
                                                lamington = FALSE),
       Budget2018_just_LITO = model_income_tax(lito_202223 = h > 6,
                                               lamington = FALSE),
       Budget2018_just_LITO_and_rates = model_income_tax(ordinary_tax_rates =  ordinary_tax_rates(h),
                                                         ordinary_tax_thresholds = ordinary_tax_thresholds(h),
                                                         lito_202223 = h > 6,
                                                         lamington = FALSE),
       Budget2018_just_lamington = model_income_tax(lamington = TRUE),
       Budget2018 = model_income_tax(ordinary_tax_rates = ordinary_tax_rates(h),
                                     ordinary_tax_thresholds = ordinary_tax_thresholds(h),
                                     lito_202223 = h > 6,
                                     lamington = TRUE))
}

Budget_1922 <- lapply(yr2fy(2019:2028), model_Budgets)
Budget_1922_Grattan <- lapply(yr2fy(2019:2028), model_Budgets, use.Treasury = FALSE)


names(Budget_1922) <- yr2fy(2019:2028)
names(Budget_1922_Grattan) <- yr2fy(2019:2028)
round(sapply(Budget_1922, sapply, revenue_foregone, USE.NAMES = TRUE) / 1e9, 2)
round(sapply(Budget_1922_Grattan, sapply, revenue_foregone, USE.NAMES = TRUE) / 1e9, 2)
sapply(Budget_1922, sapply, revenue_foregone, USE.NAMES = TRUE) %>% rowSums %>% divide_by(1e9)
sapply(Budget_1922_Grattan, sapply, revenue_foregone, USE.NAMES = TRUE) %>% rowSums %>% divide_by(1e9)

write.csv(round(sapply(Budget_1922, sapply, revenue_foregone, USE.NAMES = TRUE) / 1e9, 2), 
          "vignettes/Budget201718-summaries-2.csv")
write.csv(round(sapply(Budget_1922_Grattan, sapply, revenue_foregone, USE.NAMES = TRUE) / 1e9, 2), 
          "vignettes/Budget201718-Grattan-summaries.csv")


if (FALSE) {



s1819_19c <-
  s1819 %>%
  model_income_tax()


revenue_foregone(s1819_lito_700_60k)
revenue_foregone(s1819_lito_1000)

## ------------------------------------------------------------------------
s1819_lito_double_offset %>%
  .[Taxable_Income > 1000] %>%
  .[, new_avg_tax := new_tax / Taxable_Income] %>%
  .[, old_avg_tax := baseline_tax / Taxable_Income] %>%
  .[] %>%
  .[, Taxable_Income_percentile := weighted_ntile(Taxable_Income, n = 100)] %>%
  .[, .(new = mean(new_avg_tax),
        old = mean(old_avg_tax),
        income = mean(Taxable_Income)),
    keyby = "Taxable_Income_percentile"] %>%
  melt.data.table(id.vars = "Taxable_Income_percentile",
                  value.name = "Average tax rate") %>%
  ggplot(aes(x = Taxable_Income_percentile, y = `Average tax rate`, color = variable)) + 
  geom_line()

s1819_Budget2018 %>% 
  .[, .(Taxable_Income, baseline_tax = as.double(baseline_tax), new_tax)] %>%
  .[, delta := baseline_tax - new_tax] %>%
  melt.data.table(id.vars = c("Taxable_Income", "delta")) %>%
  .[, Taxable_Income_percentile := weighted_ntile(Taxable_Income, n = 100)] %>%
  .[, .(value = mean(value),
        delta = mean(delta),
        min_Income = min(Taxable_Income)),
    keyby = .(variable, Taxable_Income_percentile)] %>%
  .[and(variable == "baseline_tax",
        (Taxable_Income_percentile %% 20) == 10),
    label := paste0("Income: ", grattan_dollar(min_Income), "\n",
                   "Change: ", grattan_dollar(delta))] %>%
  ggplot(aes(x = Taxable_Income_percentile, y = value, color = variable)) +
  geom_point() +
  geom_label_repel(aes(label = label), nudge_y = 1, na.rm = TRUE, force = 2)

s1819_Budget2018 %>%
  .[, .(Taxable_Income = mean(Taxable_Income)),
    keyby = .(Taxable_Income_percentile = weighted_ntile(Taxable_Income, n = 100))] %>%
  .[Taxable_Income_percentile <= 95L] %>%
  .[, fill := "black"] %>%
  .[Taxable_Income_percentile %% 10 == 5, fill := "red"] %>%
  ggplot(aes(x = Taxable_Income_percentile, y = Taxable_Income, fill = fill)) + 
  scale_fill_identity() +
  geom_col(width=1) + 
  geom_hline(yintercept = seq(0, 150e3, by = 15e3),
             color = "white") +
  scale_y_continuous(labels = grattan_dollar,
                     breaks = seq(0, 150e3, by = 15e3)) + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Distribution of income (<96th percentile)")

s1819_Budget2018 %>% 
  .[, .(Taxable_Income, baseline_tax = as.integer(baseline_tax), new_tax = as.integer(new_tax))] %>%
  .[, delta := new_tax - baseline_tax] %>%
  .[, Taxable_Income_percentile := weighted_ntile(Taxable_Income, n = 100)] %>%
  .[, .(avg_delta = mean(delta)), keyby = .(Taxable_Income_percentile)] %>%
  .[] %>%
  ggplot(aes(x = Taxable_Income_percentile, y = avg_delta)) + 
  geom_col() + 
  scale_y_continuous(breaks = c(0, -200, -400, -555)) + 
  ggtitle("Average extra tax collected by taxable income percentile")

round_to_nearest <- function(x, nrst) {
  nrst * round(x / nrst)
}

s1819_Budget2018 %>% 
  .[, .(Taxable_Income,
        baseline_tax = as.integer(baseline_tax),
        new_tax = as.integer(new_tax))] %>%
  .[, delta := new_tax - baseline_tax] %>%
  .[, .(avg_delta = mean(delta)), keyby = .(Taxable_Income = round_to_nearest(Taxable_Income, 5000))] %>%
  .[Taxable_Income < 100e3] %>%
  ggplot(aes(x = Taxable_Income, y = avg_delta)) + 
  geom_col() + 
  scale_y_continuous(breaks = c(0, -200, -400, -555)) +
  scale_x_continuous(breaks = seq(0, 100e3, by = 5000), labels = grattan_dollar) + 
  ggtitle("Average extra tax collected by taxable income rounded to nearest $5000")

s1819_Budget2018 %>% 
  .[, .(N = sum(WEIGHT)), keyby = .(Taxable_Income = round_to_nearest(Taxable_Income, 5000))] %>%
  .[Taxable_Income < 105e3] %>%
  ggplot(aes(x = Taxable_Income, y = N)) + 
  geom_col() + 
  scale_y_continuous("Number taxpayers", label = comma) +
  scale_x_continuous(breaks = seq(0, 100e3, by = 5000), labels = grattan_dollar) + 
  ggtitle("Number of taxpayers per $5000 income increment")

s1819_Budget2018 %>% 
  .[, .(Taxable_Income, WEIGHT, baseline_tax = as.integer(baseline_tax), new_tax = as.integer(new_tax))] %>%
  .[, delta := new_tax - baseline_tax] %>%
  .[, .(total_delta = sum(delta * WEIGHT)),
    keyby = .(Taxable_Income = round_to_nearest(Taxable_Income, 5000))] %>%
  .[Taxable_Income < 100e3] %>%
  ggplot(aes(x = Taxable_Income, y = -total_delta)) + 
  geom_col() + 
  scale_y_continuous(labels = grattan_dollar) +
  scale_x_continuous(breaks = seq(0, 100e3, by = 5000), labels = grattan_dollar) + 
  ggtitle("Total revenue foregone for each $5000 block")

## ----sample_file_1819----------------------------------------------------
sample_file_1819 <-
  project(sample_file_1516,
          h = 3L,
          fy.year.of.sample.file = "2015-16")

## ----sample_file_2122----------------------------------------------------
sample_file_2122 <-
  project(sample_file_1516,
          h = 6L,
          fy.year.of.sample.file = "2015-16")

## ----add-tax-paid-avg-tax------------------------------------------------
sample_file_1819[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_1819),
                                          fy.year = "2018-19")]
sample_file_1819[, avg_tax := tax_paid / Taxable_Income]
sample_file_2122[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_2122),
                                          fy.year = "2019-20")]
sample_file_2122[, avg_tax := tax_paid / Taxable_Income]

## ----avg_tax_by_decile---------------------------------------------------
avg_tax_by_decile_1819 <- 
  sample_file_1819 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

avg_tax_by_decile_2122 <- 
  sample_file_2122 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

## ----tax-changes-grattan-forecast----------------------------------------
avg_tax_by_decile_1819[avg_tax_by_decile_2122] %>%
  .[decile > 1] %>%
  .[, ppt_increase := 100*(i.avg_tax - avg_tax)] %>%
  .[, decile := factor(decile)] %>%
  ggplot(aes(x = decile, y = ppt_increase)) + 
  geom_col()

## ----Budget_wage_series--------------------------------------------------
Budget_wage_series <-
  data.table(fy_year = c("2017-18", "2018-19", "2019-20", "2020-21"),
             r = c(0.025, 0.03, 0.035, 0.0375))

kable(Budget_wage_series)

## ----project-with-respect-to-budget--------------------------------------
sample_file_1617 <- project(sample_file_1213,
                            h = 4L,
                            fy.year.of.sample.file = "2012-13")

sample_file_2021 <- project(sample_file_1213,
                            fy.year.of.sample.file = "2012-13",
                            h = 8L,
                            wage.series = Budget_wage_series)

sample_file_1617[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_1617),
                                          fy.year = "2016-17")]
sample_file_1617[, avg_tax := tax_paid / Taxable_Income]
sample_file_2021[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_2021),
                                          fy.year = "2019-20")]
sample_file_2021[, avg_tax := tax_paid / Taxable_Income]

avg_tax_by_decile_1617 <- 
  sample_file_1617 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

avg_tax_by_decile_2021 <- 
  sample_file_2021 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

difference_2021_Budget <-
  avg_tax_by_decile_1617[avg_tax_by_decile_2021] %>%
  .[decile > 1] %>%
  .[, ppt_increase := 100*(i.avg_tax - avg_tax)]

difference_2021_Budget %>%
  copy %>%
  .[, decile := factor(decile)] %>%
  ggplot(aes(x = decile, y = ppt_increase)) + 
  geom_col()
}