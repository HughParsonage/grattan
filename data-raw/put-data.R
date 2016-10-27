# Income tax tables.
library(magrittr)
library(dplyr)
library(forecast)
library(data.table)
library(dtplyr)
library(taxstats)
library(grattan)

renew = FALSE

tax_tbl <-
  data.table::fread("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv")

lito_tbl <- 
  readxl::read_excel("./data-raw/lito-info.xlsx", sheet = 1) %>% dplyr::select(-source)

lito_tbl %>% 
  readr::write_tsv("./data-raw/lito-info.tsv")

medicare_tbl_indiv <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.)

medicare_tbl <- 
  medicare_tbl_indiv %>%
  dplyr::mutate_each(funs(as.logical), sato, pto, sapto) %>%
  data.table::as.data.table(.) %>%
  data.table::setkey(fy_year, sapto, family_status) %>%
  # avoid cartesian joins
  unique

# To ensure faster versions of calculations do not evaluate NA.
set(medicare_tbl, which(is.na(medicare_tbl[["lower_up_for_each_child"]])), "lower_up_for_each_child", 0)
set(medicare_tbl, which(is.na(medicare_tbl[["upper_threshold"]])), "upper_threshold", 0)

medicare_tbl %>% 
  readr::write_tsv("./data-raw/medicare_tbl.tsv")

sapto_tbl <- 
  readxl::read_excel("./data-raw/SAPTO-rates.xlsx", sheet = 1) %>% 
  data.table::as.data.table(.) %>% 
  data.table::setkey(fy_year, family_status) %>% 
  # avoid cartesian joins in income_tax
  unique

sapto_tbl %>%
  readr::write_tsv("./data-raw/sapto_tbl.tsv")

hecs_tbl <- 
  openxlsx::readWorkbook("./data-raw/Historical HELP thresholds 1989 to 2015.xlsx", 
                         sheet = "Actual historical threshold", 
                         rows = c(4:33), check.names = FALSE, colNames = TRUE) %>%
  # repayment rate looks like "7.5%" in Excel which is stored as 0.075 (approximately)
  tidyr::gather(repayment_rate, repayment_threshold, -Year) %>%
  dplyr::mutate(repayment_rate = as.numeric(repayment_rate)) %>%
  dplyr::filter(!is.na(repayment_threshold)) %>%
  data.table::as.data.table(.) %>%
  data.table::setnames(., "Year", "fy_year") %>%
  # need to explicitly state 0.
  {
    data.table::rbindlist(list(., data.table::data.table(fy_year = unique(.$fy_year), repayment_rate = 0, repayment_threshold = -Inf))) 
  } %>%
  data.table::setkey(fy_year, repayment_threshold)

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
  data.table::fread("./data-raw/lf-trend.tsv", select = c("obsTime", "obsValue"))

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
  
  alien.cols <- col.names[!col.names %in% names(taxstats::sample_file_1314)]
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

generic_inflators <- if (!renew) fread("./data-raw/generic_inflators.tsv") else {
  lapply(1:10, 
         function(h) dplyr::mutate(generic_inflator(vars = generic.cols, h = h, fy.year.of.sample.file = "2013-14"), 
                                   H = h)) %>% 
  rbindlist(use.names = TRUE) %>%
  mutate(fy_year = yr2fy(2014 + H)) %>%
  rename(h = H)
}

cg_inflators_1314 <- if (!renew) fread("./data-raw/cg_inflators_1314.tsv") else {
  cg_table <- 
    taxstats::sample_files_all %>%
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
    data.table::as.data.table(taxstats::individuals_table1_201314) %>%
    dplyr::filter(Selected_items == "Net capital gain") %>%
    dplyr::filter(!is.na(Count))  %>%
    dplyr::select(fy_year, n_CG = Count)
  
  n_cg <- 
    rbind(n_cg_history, 
          data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                     n_CG = as.numeric(forecast(n_cg_history$n_CG)$mean))
          )
  
  cg <- 
    rbind(as.data.table(cg_table), 
          data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                     zero_discount_Net_CG_total = as.numeric(forecast(cg_table$zero_discount_Net_CG_total)$mean))
    )
  
  cg_inf <- 
    merge(n_cg, cg, by.x = "fy_year", by.y = "fy.year") %>% 
    as.data.table
  
  cg_inf[, cg_inflator := zero_discount_Net_CG_total / n_CG]
  cg_inf[, cg_inflator := cg_inflator / cg_inflator[cg_inf$fy_year == "2013-14"]]
  cg_inf
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
base_rates_Age_pension_by_year <- 
  fread("data-raw/max-basic-rates-of-pension-1963-2016.csv") %>%
  mutate_all(funs(rm_comma)) %>%
  setnames(1, "Date") %>%
  mutate(Date = gsub(" Note .*$", "", Date, perl = TRUE), 
         Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(`Standard rate` != "Standard rate") %>%
  select(Date, `Standard rate`, `Married rate`) %>%
  mutate_each(funs(as.numeric), -Date) %>%
  filter(complete.cases(.))

devtools::use_data(lito_tbl, 
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
                   generic_inflators,
                   cg_inflators_1314,
                   super_contribution_inflator_1314,
                   #
                   salary_by_fy_swtile,
                   differential_sw_uprates,
                   # possibly separable
                   .avbl_fractions,
                   base_rates_Age_pension_by_year,
                   
                   internal = TRUE, overwrite = TRUE)
