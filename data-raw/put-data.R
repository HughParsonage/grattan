# Data for internal use
# Must be sourced after modification
library(magrittr)
library(dplyr)
library(forecast)
library(data.table)
library(tidyr)
library(dtplyr)
library(taxstats)
sample_files_all <- get_sample_files_all()
library(grattan)
library(readr)
library(readxl)
library(openxlsx)
options("scipen" = 99)
if (packageVersion("data.table") < package_version("1.9.8")){
  fwrite <- function(..., sep = "\t") readr::write_tsv(...)
}

renew <- TRUE

tax_tbl <- 
  lapply(yr2fy(1990:2020), 
         function(fy_year) {
           read_excel("./data-raw/tax_brackets_and_marginal_rates.xlsx", sheet = fy_year) %>% 
             mutate(fy_year = fy_year) %>% 
             as.data.table
         })  %>%
  lapply(as.data.table) %>%
  rbindlist %>%
  setnames("lower_brackets", "lower_bracket") %>%
  select(fy_year, lower_bracket, marginal_rate) %>%
  fwrite("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv", sep = "\t")

tax_tbl <-
  data.table::fread("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv")

# tax_table2 provides the raw tax tables, but also the amount
# of tax paid at each bracket, to make the rolling join 
# calculation later a one liner.
tax_table2 <- 
  tax_tbl %>%
  dplyr::group_by(fy_year) %>%
  dplyr::mutate(
    tax_at = cumsum(data.table::shift(marginal_rate, type = "lag", fill = 0) * (lower_bracket - data.table::shift(lower_bracket, type = "lag", fill = 0))),
    income = lower_bracket) %>%
  dplyr::select(fy_year, income, lower_bracket, marginal_rate, tax_at) %>%
  data.table::as.data.table(.) %>%
  data.table::setkey(fy_year, income) 

lito_tbl <- 
  readxl::read_excel("./data-raw/lito-info.xlsx", sheet = 1) %>% 
  dplyr::select(-source) %>%
  as.data.table %>%
  setkey("fy_year")

lito_tbl %>% 
  readr::write_tsv("./data-raw/lito-info.tsv")

medicare_tbl_indiv <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.)

medicare_tbl <- 
  medicare_tbl_indiv %>%
  dplyr::mutate_each(funs(as.logical), sato, pto, sapto) %>%
  data.table::as.data.table(.) %>%
  # mutate(family_status = "individual") %>%
  data.table::setkey(fy_year, sapto) %>%
  # avoid cartesian joins
  unique()

# To ensure faster versions of calculations do not evaluate NA.
set(medicare_tbl, which(is.na(medicare_tbl[["lower_up_for_each_child"]])), "lower_up_for_each_child", 0)
set(medicare_tbl, which(is.na(medicare_tbl[["upper_threshold"]])), "upper_threshold", 0)

medicare_tbl %>% 
  readr::write_tsv("./data-raw/medicare_tbl.tsv")

sapto_tbl <- 
  readxl::read_excel("./data-raw/SAPTO-rates.xlsx", sheet = 1) %>% 
  data.table::as.data.table(.) %>% 
  mutate(max_offset = if_else(family_status == "single", max_offset, max_offset * 2),
         lower_threshold = if_else(family_status == "single", lower_threshold, lower_threshold * 2)) %>%
  # Choose maximum
  group_by(fy_year, family_status) %>%
  filter(max_offset == max(max_offset)) %>%
  as.data.table %>%
  data.table::setkey(fy_year, family_status) %>% 
  # avoid cartesian joins in income_tax
  unique

sapto_tbl %>%
  readr::write_tsv("./data-raw/sapto_tbl.tsv")

hecs_tbl <- 
  fread("./data-raw/Student-repayment-thresholds-HELP.tsv") %>%
  setkey(fy_year, repayment_threshold)

# Manually
cpi_unadj <- 
  data.table::fread("./data-raw/cpi-unadjusted-manual.tsv", select = c("obsTime", "obsValue"))

cpi_seasonal_adjustment <- 
  data.table::fread("./data-raw/cpi-seasonally-adjusted-manual.tsv", select = c("obsTime", "obsValue")) 

cpi_trimmed <-
  data.table::fread("./data-raw/cpi-trimmed-mean-manual.tsv", select = c("obsTime", "obsValue"))

wages_trend <- 
  data.table::fread("./data-raw/wages-trend.tsv", select = c("obsTime", "obsValue"))

lf_trend <- 
  tryCatch({
    lf.url.trend <- 
      # "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978-02"
      "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978"
    lf <- rsdmx::readSDMX(lf.url.trend)
    lf <- 
      as.data.frame(lf) %>% 
      as.data.table %>%
      select(obsTime, obsValue) %T>%
      fwrite("./data-raw/lf-trend.tsv", sep = "\t")
  }, 
    error = function(e){
      data.table::fread("./data-raw/lf-trend.tsv" 
                        , select = c("obsTime", "obsValue"))
    })

cgt_expenditures <- 
  data.table::fread("./data-raw/tax-expenditures-cgt-historical.tsv")

{
  col.names <- names(sample_file_1314)
  
  wagey.cols <- c("Sw_amt", 
                  "Alow_ben_amt",
                  "ETP_txbl_amt",
                  "Rptbl_Empr_spr_cont_amt", 
                  "Non_emp_spr_amt", 
                  "MCS_Emplr_Contr", 
                  "MCS_Prsnl_Contr", 
                  "MCS_Othr_Contr")
  
  super.bal.col <- c("MCS_Ttl_Acnt_Bal")
  
  lfy.cols <- c("WEIGHT")
  
  cpiy.cols <- c(grep("WRE", col.names, value = TRUE), # work-related expenses
                 "Cost_tax_affairs_amt",
                 "Other_Ded_amt")
  
  derived.cols <- c("Net_rent_amt",
                    "Net_PP_BI_amt",
                    "Net_NPP_BI_amt",
                    "Tot_inc_amt",
                    "Tot_ded_amt",
                    "Taxable_Income")
  
  CGTy.cols <- c("Net_CG_amt", "Tot_CY_CG_amt")
  
  alien.cols <- col.names[!col.names %in% names(sample_file_1314)]
  Not.Inflated <- c("Ind", 
                    "Gender",
                    "age_range", 
                    "Occ_code", 
                    "Partner_status", 
                    "Region", 
                    "Lodgment_method", 
                    "PHI_Ind", 
                    alien.cols)
  
  SetDiff <- function(...) Reduce(setdiff, list(...), right = FALSE)
  
  generic.cols <<- SetDiff(col.names, 
                           wagey.cols, super.bal.col, lfy.cols, cpiy.cols, derived.cols, Not.Inflated)
  }

select_which_ <- function(...) grattan:::select_which_(...)

MeanNumeric <- function(x){
  sum(as.numeric(x)) / length(x)
}

mean_of_nonzero <- function (x) {
  MeanNumeric(x[x > 0])
}


mean_of_each_taxstats_var <- 
  sample_files_all %>%
  select_(.dots = c("fy.year", names(.))) %>%
  select_which_(is.numeric, "fy.year") %>%
  group_by_("fy.year") %>%  
  summarise_each(funs(MeanNumeric)) 

meanPositive_of_each_taxstats_var <- 
  sample_files_all %>%
  select_(.dots = c("fy.year", names(.))) %>%
  select_which_(is.numeric, "fy.year") %>%
  group_by_("fy.year") %>%  
  summarise_each(funs(mean_of_nonzero))

generic_inflators_1314 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1314.tsv")
  }  else {
    lapply(1:10, 
           function(h) dplyr::mutate(grattan:::generic_inflator(vars = generic.cols, h = h, fy.year.of.sample.file = "2013-14"), 
                                     H = h)) %>% 
      rbindlist(use.names = TRUE) %>%
      mutate(fy_year = yr2fy(2014 + H)) %>%
      rename(h = H) %T>%
      write_tsv("./data-raw/generic_inflators_1314.tsv") %>%
      .[]
  }

generic_inflators_1213 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1213.tsv")
  }  else {
    lapply(1:10, 
           function(h) dplyr::mutate(grattan:::generic_inflator(vars = generic.cols, h = h, fy.year.of.sample.file = "2012-13"), 
                                     H = h)) %>% 
      rbindlist(use.names = TRUE) %>%
      mutate(fy_year = yr2fy(2013 + H)) %>%
      rename(h = H) %T>%
      write_tsv("./data-raw/generic_inflators_1213.tsv") %>%
      .[]
  }

cg_inflators_1314 <- if (!renew) fread("./data-raw/cg_inflators_1314.tsv") else {
  get_cg_inf <- function(series = "mean"){
    cg_table <- 
      sample_files_all %>%
      dplyr::select(fy.year, Taxable_Income, Net_CG_amt) %>%
      dplyr::filter(Net_CG_amt > 0) %>%
      dplyr::mutate(marginal_rate_first = income_tax(Taxable_Income + 1, 
                                                     fy.year = fy.year) - income_tax(Taxable_Income, 
                                                                                     fy.year = fy.year)) %>%
      dplyr::mutate(marginal_rate_last = (income_tax(Taxable_Income + Net_CG_amt, fy.year = fy.year) - income_tax(Taxable_Income, fy.year = fy.year)) / Net_CG_amt) %>%
      dplyr::group_by(fy.year) %>%
      dplyr::summarise(mean_mr1 = mean(marginal_rate_first), 
                       mean_wmr1 = stats::weighted.mean(marginal_rate_first, Net_CG_amt), 
                       mean_mrL = mean(marginal_rate_last), 
                       mean_wmrL = stats::weighted.mean(marginal_rate_last, Net_CG_amt)) %>% 
      merge(grattan:::cgt_expenditures, by.x = "fy.year", by.y = "FY", all = TRUE) %>% 
      # dplyr::select_(.dots = c(-URL, -Projected) %>%
      grattan:::unselect_(.dots = c("URL", "Projected")) %>%
      dplyr::rename(revenue_foregone = CGT_discount_for_individuals_and_trusts_millions) %>%
      dplyr::mutate(revenue_foregone = revenue_foregone * 10^6,
                    zero_discount_Net_CG_total = revenue_foregone / mean(mean_wmrL, na.rm = TRUE)) %>%
      dplyr::select(fy.year, zero_discount_Net_CG_total)
    
    n_cg_history <- 
      data.table::as.data.table(individuals_table1_201314) %>%
      dplyr::filter(Selected_items == "Net capital gain") %>%
      dplyr::filter(!is.na(Count))  %>%
      dplyr::select(fy_year, n_CG = Count)
    
    forecaster <- function(...) forecast(...)  # gforecast terrible
    
    switch(series,
           "mean" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$mean))
               )
             
             cg <- 
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$mean)))
             }, 
             "lower" = {
               n_cg <- 
                 rbind(n_cg_history, 
                       data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                  n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$lower))
                 )
               
               cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$lower)))
             }, 
             "upper" = {
               n_cg <- 
                 rbind(n_cg_history, 
                       data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                  n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$upper))
                 )
               
               cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$upper)))
             })
    
    cg_inf <- 
      merge(n_cg, cg, by.x = "fy_year", by.y = "fy.year") %>% 
      as.data.table
    
    cg_inf[, cg_inflator := zero_discount_Net_CG_total / n_CG]
    cg_inf[, cg_inflator := cg_inflator / cg_inflator[cg_inf$fy_year == "2013-14"]]
    cg_inf[, forecast.series := series]
  }
  lapply(c("lower", "mean", "upper"), get_cg_inf) %>%
    rbindlist(use.names = TRUE) %T>%
    write_tsv("./data-raw/cg_inflators_1314.tsv") %>%
    .[]
}

cg_inflators_1213 <- 
  if (!renew) {
    fread("./data-raw/cg_inflators_1213.tsv") 
  } else {
  get_cg_inf <- function(series = "mean"){
    cg_table <- 
      sample_files_all %>%
      dplyr::select(fy.year, Taxable_Income, Net_CG_amt) %>%
      dplyr::filter(Net_CG_amt > 0) %>%
      dplyr::mutate(marginal_rate_first = income_tax(Taxable_Income + 1, 
                                                     fy.year = fy.year) - income_tax(Taxable_Income, 
                                                                                     fy.year = fy.year)) %>%
      dplyr::mutate(marginal_rate_last = (income_tax(Taxable_Income + Net_CG_amt, fy.year = fy.year) - income_tax(Taxable_Income, fy.year = fy.year)) / Net_CG_amt) %>%
      dplyr::group_by(fy.year) %>%
      dplyr::summarise(mean_mr1 = mean(marginal_rate_first), 
                       mean_wmr1 = stats::weighted.mean(marginal_rate_first, Net_CG_amt), 
                       mean_mrL = mean(marginal_rate_last), 
                       mean_wmrL = stats::weighted.mean(marginal_rate_last, Net_CG_amt)) %>% 
      merge(grattan:::cgt_expenditures, by.x = "fy.year", by.y = "FY", all = TRUE) %>% 
      # dplyr::select_(.dots = c(-URL, -Projected) %>%
      grattan:::unselect_(.dots = c("URL", "Projected")) %>%
      dplyr::rename(revenue_foregone = CGT_discount_for_individuals_and_trusts_millions) %>%
      dplyr::mutate(revenue_foregone = revenue_foregone * 10^6,
                    zero_discount_Net_CG_total = revenue_foregone / mean(mean_wmrL, na.rm = TRUE)) %>%
      dplyr::select(fy.year, zero_discount_Net_CG_total)
    
    n_cg_history <- 
      data.table::as.data.table(individuals_table1_201314) %>%
      dplyr::filter(Selected_items == "Net capital gain") %>%
      dplyr::filter(!is.na(Count))  %>%
      dplyr::select(fy_year, n_CG = Count)
    
    forecaster <- function(object, ...) forecast(object, h = 11, ...)  # gforecast terrible
    
    switch(series,
           "mean" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 0:10),
                                n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$mean))
               )
             
             cg <- 
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 0:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$mean)))
           }, 
           "lower" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 0:10),
                                n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$lower))
               )
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 0:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$lower)))
           }, 
           "upper" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 0:10),
                                n_CG = as.numeric(forecaster(n_cg_history$n_CG, level = 95)$upper))
               )
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 0:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$upper)))
           })
    
    cg_inf <- 
      merge(n_cg, cg, by.x = "fy_year", by.y = "fy.year") %>% 
      as.data.table
    
    cg_inf[, cg_inflator := zero_discount_Net_CG_total / n_CG]
    cg_inf[, cg_inflator := cg_inflator / cg_inflator[cg_inf$fy_year == "2012-13"]]
    cg_inf[, forecast.series := series]
  }
  lapply(c("lower", "mean", "upper"), get_cg_inf) %>%
    rbindlist(use.names = TRUE) %T>%
    write_tsv("./data-raw/cg_inflators_1213.tsv") %>%
    .[]
}
  
  super_contribution_inflator_1314 <- 
  {
  sample_file_1314_concessional_contribution_total <- 
    
    apply_super_caps_and_div293(.sample.file = sample_file_1314,
                                # 2013-14 policy settings
                                div293_threshold = 300e3, 
                                cap = 30e3, cap2 = 35e3, age_based_cap = TRUE, cap2_age = 49) %$%
    sum(concessional_contributions) * 50
  
  # What do the ATO aggregate tables suggest should be the answer?
  library(taxstats)
  funds <- 
    funds_table1_201314 %>%
    filter(Selected_items == "Assessable contributions") %>%
    select(fy_year, Assessable_contributions_funds = Sum) %>%
    setkey(fy_year)
  
  smsfs <- 
    funds_table2_smsf_201314 %>%
    filter(Selected_items == "Assessable contributions") %>%
    select(fy_year, Assessable_contributions_smsfs = Sum) %>%
    setkey(fy_year)
  
  ato_aggregate_contributions <- 
    smsfs[funds] %>%
    mutate(total_contributions = Assessable_contributions_smsfs + Assessable_contributions_funds)
  
  ato_aggregate_contributions[fy_year == "2013-14"][["total_contributions"]] / sample_file_1314_concessional_contribution_total
}

# differential uprating
salary_by_fy_swtile <- 
  sample_files_all %>%
  select(fy.year, Sw_amt) %>%
  filter(Sw_amt > 0) %>%
  group_by(fy.year) %>%
  mutate(Sw_amt_percentile = ntile(Sw_amt, 100)) %>%
  ungroup %>%
  group_by(fy.year, Sw_amt_percentile) %>%
  summarise(average_salary = mean(Sw_amt), 
            min_salary = min(Sw_amt)) %>%
  ungroup %>%
  setkey(Sw_amt_percentile)
  

differential_sw_uprates <- 
  salary_by_fy_swtile %>%
  ungroup %>%
  arrange(Sw_amt_percentile, fy.year) %>%
  group_by(Sw_amt_percentile) %>%
  mutate(r_average_salary = average_salary / lag(average_salary) - 1) %>%
  filter(fy.year != min(fy.year)) %>%
  group_by(Sw_amt_percentile) %>%
  summarise(avg_r = mean(r_average_salary)) %>% 
  mutate(uprate_factor_raw = avg_r / mean(avg_r)) %>%
  select(Sw_amt_percentile, uprate_factor_raw) %>%
  setkey(Sw_amt_percentile) %>%
  # Span = 0.5 seems to be the point at which the curve has only 
  # one local extremum.
  # ggplot(., aes(x = Sw_amt_percentile, y = uprate_factor)) + geom_point() + stat_smooth(method = "loess", span = 0.45)
  mutate(pred_loess = predict(loess(uprate_factor_raw ~ Sw_amt_percentile, data = ., span = 0.45), newdata = .)) %>% 
  rename(uprate_factor = pred_loess)

.avbl_fractions <-
  # map between common fraction and English
  data.table(val = c(1/10, 1/5, 1/4, 1/3, 1/2, 2/3, 3/4), 
             txt = c("one-tenth", "one-fifth", "one-quarter", 
                     "one-third", "one-half",
                     "two-thirds", "three-quarters"), 
             Txt = c("One-tenth", "One-fifth", "One-quarter", 
                     "One-third", "One-half",
                     "Two-thirds", "Three-quarters"))
rm_comma <- function(x) gsub("[^\\.0-9]", "", gsub(",", "", x, fixed = TRUE))

# http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
Age_pension_base_rates_by_year <- 
  if (!file.exists("data-raw/max-basic-rates-of-pension-1963-2016.tsv")){
    fread("data-raw/max-basic-rates-of-pension-1963-2016.csv") %>%
      mutate_all(funs(rm_comma)) %>%
      setnames(1, "Date") %>%
      mutate(Date = gsub(" Note .*$", "", Date, perl = TRUE), 
             Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      filter(`Standard rate` != "Standard rate") %>%
      select(Date, `Standard rate`, `Married rate`) %>%
      mutate_each(funs(as.numeric), -Date) %>%
      filter(complete.cases(.)) %T>%
      write_tsv("data-raw/max-basic-rates-of-pension-1963-2016.tsv") %>%
      .[]
  } else {
    fread("data-raw/max-basic-rates-of-pension-1963-2016.tsv")
  }

Age_pension_assets_test_by_year <- 
  read_excel("data-raw/Age-Pension-assets-test-1997-2016.xlsx", sheet = "clean") %>%
  gather(type, assets_test, -Date) %>%
  mutate(type = gsub("[^A-Za-z]", " ", type))

bto_tbl <- 
  read_excel("data-raw/beneficiary-tax-offset-by-fy.xlsx") %>%
  as.data.table %>%
  setkey(fy_year)

Age_pension_permissible_income_by_Date <- 
  read_excel("data-raw/age-pension-permissible-income.xlsx") %>%
  gather(type, permissible_income, -Date) %>%
  mutate(type = trimws(gsub("Permissible income ", "", gsub("[^A-Za-z]", " ", type))))

aust_pop_by_age_yearqtr <- 
  fread("./data-raw/Estim-Resi-Pop-by-age-1981-2016.csv", 
        select = c("Age", "Time", "Value")) %>%
  mutate(Date = as.Date(paste0("01-", Time), format = "%d-%b-%y")) %>%
  select(Age, Date, Value) %>%
  setkey(Age, Date)


# download.file(url = "http://www.ausstats.abs.gov.au/ausstats/meisubs.nsf/LatestTimeSeries/5206001_key_aggregates/$FILE/5206001_key_aggregates.xlsx", destfile = "")
abs_key_aggregates_names <- 
  read.xlsx("./data-raw/5206001_key_aggregates-2016-12-07-release.xlsx", sheet = "Data1", rows = c(2L, 10L))

units_by_series_id <- 
  data.table(Series_ID = as.character(abs_key_aggregates_names), 
             units = names(abs_key_aggregates_names)) %>%
  .[-1] %>%  # 'Series ID' and 'units'
  mutate(multiply = if_else(grepl("million", units, ignore.case = TRUE), 
                            10^6, 
                            if_else(grepl("(percent)|(proportion)", units, ignore.case = TRUE), 
                                    0.01, 
                                    1)))

abs_key_aggregates <- 
  read_excel("data-raw/5206001_key_aggregates-2016-12-07-release.xlsx", sheet = "Data1", skip = 9) %>%
  as.data.table %>%
  melt.data.table(id.vars = "Series ID", variable.name = "Series_ID") %>%
  setnames("Series ID", "Date") %>%
  merge(units_by_series_id, by = "Series_ID", sort = FALSE) %>%
  mutate(value = value * multiply, 
         Date = as.Date(Date)) %>%
  select(-units, -multiply) %>%
  # to limit size to only what is used
  filter(Series_ID %in% c( "A2304350J"  # GDP
                          ,"A2304354T"  # GNI
                          ))

read_csv_2col <- function(...) {
  suppressMessages(suppressWarnings(read_csv(...))) %>% select(1:2) %>% setDT
}

abs_residential_property_price_Syd <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppisrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Syd")
  
abs_residential_property_price_Mel <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppimrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Mel")
  
abs_residential_property_price_Bne <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppibrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Bne")
  
abs_residential_property_price_Per <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppiprppiinpcoq.csv") %>%
  setnames(names(.)[2], "Per")
  
abs_residential_property_price_Adl <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppiprppiinpcoq.csv") %>%
  select(1:2) %>%
  setnames(names(.)[2], "Adl")
  
abs_residential_property_price_Hob <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppihrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Hob")
  
abs_residential_property_price_Cbr <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppicrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Cbr")
  
abs_residential_property_price_Drw <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppidrppiinpcoq.csv") %>%
  setnames(names(.)[2], "Drw")
  
abs_residential_property_price_AVG <-
  read_csv_2col("http://ausmacrodata.org/Data/6416.0/rppiwaeccrppiinpcoq.csv") %>%
  setnames(names(.)[2], "AVG")

residential_property_prices <- 
  Reduce(f = function(X, Y) merge(X, Y, by = names(X)[1]),
         mget(ls(pattern = "^abs_residential_property_"))) %>%
  setnames(1, "Date") %>%
  mutate(Date = as.Date(paste0("01/", Date), "%d/%m/%Y")) %>%
  select(Date,
         Sydney = Syd,
         Melbourne = Mel,
         Brisbane = Bne,
         Perth = Per,
         Adelaide = Adl,
         Hobart = Hob,
         Canberra = Cbr,
         Darwin = Drw,
         `Australia (weighted average)` = AVG) %>%
  .[order(Date)] %>%
  melt.data.table(id.vars = "Date", variable.name = "City", value.name = "Residential_property_price_index")

devtools::use_data(residential_property_prices)


devtools::use_data(tax_table2, 
                   lito_tbl, 
                   tax_tbl, 
                   medicare_tbl, 
                   sapto_tbl,
                   hecs_tbl,
                   cpi_unadj,
                   cpi_seasonal_adjustment,
                   cpi_trimmed,
                   wages_trend,
                   lf_trend,
                   cgt_expenditures,
                   mean_of_each_taxstats_var, 
                   meanPositive_of_each_taxstats_var,
                   generic_inflators_1314,
                   generic_inflators_1213,
                   cg_inflators_1314,
                   cg_inflators_1213,
                   super_contribution_inflator_1314,
                   #
                   salary_by_fy_swtile,
                   differential_sw_uprates,
                   # possibly separable
                   .avbl_fractions,
                   Age_pension_base_rates_by_year,
                   Age_pension_assets_test_by_year,
                   Age_pension_permissible_income_by_Date,
                   bto_tbl,
                   aust_pop_by_age_yearqtr,
                   abs_key_aggregates,
                   
                   internal = TRUE, overwrite = TRUE)
