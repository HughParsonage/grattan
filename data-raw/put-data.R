# Data for internal use
# Must be sourced after modification
if ("dplyr" %in% .packages()) {
  stop("dplyr is attached. Restart R and source.")
}
library(unpivotr)
library(htmltab)
library(rsdmx)
if (packageVersion("rsdmx") < package_version("0.5.10")) {
  warning("rsdmx will not properly connect to ABS for versions of rsdmx before 0.5.10")
}
library(magrittr)
library(dplyr)
library(forecast)
library(data.table)
library(tidyr)
library(dtplyr)
library(taxstats)

if (requireNamespace("SampleFile1415", quietly = TRUE)) {
  sample_file_1415 <- as.data.table(SampleFile1415::sample_file_1415)
  sample_file_1415[, "fy.year" := "2014-15"]
  sample_file_1415[, WEIGHT := 50L]
  sample_files_all <-
    rbindlist(list(get_sample_files_all(),
                   sample_file_1415), 
              use.names = TRUE, 
              fill = TRUE)
} else {
  stop("SampleFile1415 package is required to run put-data.R.")
}

DropboxInfo <- 
  if (Sys.getenv("OS") == "Windows_NT") {
    file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox", "info.json")
  } else {
    "~/.dropbox/info.json"
  }

if (file.exists(DropboxInfo)) {
  Path2Dropbox <- 
    jsonlite::fromJSON(DropboxInfo) %>%
    use_series("business") %>%
    use_series("path")
}

library(ozTaxData) # devtools::install_github("hughparsonage/ozTaxData)
if(exists('sample_15_16', where = 'package:ozTaxData')){
  sample_file_1516 <- as.data.table(ozTaxData::sample_15_16)
  
} else {
  # Define the local path to the 2015-16 sample file here, if it's not in ozTaxData
  local_1516_path <- file.path(Path2Dropbox,
                               "Matt Cowgill",
                               "Tax/data/ATO/2016 sample file/2016_sample_file.csv")
  sample_file_1516 <- fread(file = local_1516_path)
}

sample_file_1516[, fy.year := "2015-16"]
sample_file_1516[, WEIGHT := 50]

if (file.exists(local_1617_path <- "../taxstats1617/2017_sample_file.csv")) {
  sample_file_1617 <- fread(file = local_1617_path)
} else if (file.exists(local_1617_path <- file.path(Path2Dropbox, 
                                             'Future Piggy Bank\\Data and Analysis\\Taxation\\ATO sample files\\2017 sample file', '2017_sample_file.csv'))) {
  sample_file_1617 <- fread(file = local_1617_path, sep = ",")
}
sample_file_1617[, fy.year := "2016-17"]
sample_file_1617[, WEIGHT := 50]


sample_files_all <- rbindlist(list(sample_files_all,
                                   sample_file_1516,
                                   sample_file_1617),
                              use.names = TRUE,
                              fill = TRUE)


#library(grattan)
devtools::load_all(".")
library(readr)
library(readxl)
legacy_repair <- function(nms, prefix = "X", sep = "__") {
  if (length(nms) == 0) return(character())
  blank <- nms == ""
  nms[!blank] <- make.unique(nms[!blank], sep = sep)
  new_nms <- setdiff(paste(prefix, seq_along(nms), sep = sep), nms)
  nms[blank] <- new_nms[seq_len(sum(blank))]
  nms
}
library(default)
default(read_excel) <- list(.name_repair = legacy_repair)
library(tidyxl)
library(hutils)
options("scipen" = 99)
if (packageVersion("data.table") < package_version("1.9.8")){
  fwrite <- function(..., sep = "\t") readr::write_tsv(...)
}

git_compare_prev <- function(file.tsv, nomatch = NA_integer_, threshold = 0) {
  if (file.exists(file.tsv) && 
      endsWith(dirname(file.tsv), "sysdata")) {
    file.tsv <- basename(file.tsv)
  }
  shell(paste0("git show HEAD~:data-raw/sysdata/", file.tsv, " > tempshow.tsv"))
  prev <- fread("tempshow.tsv")
  unlink("tempshow.tsv")
  curr <- fread(file = paste0("data-raw/sysdata/", file.tsv))
  out <- 
    if (anyNA(nomatch)) {
      prev[curr, on = "obsTime"] 
    } else {
      prev[curr, on = "obsTime", nomatch = 0L]
    }
  
  
}

renew <- FALSE

tax_tbl <-
  data.table::fread("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv")

# tax_table2 provides the raw tax tables, but also the amount
# of tax paid at each bracket, to make the rolling join 
# calculation later a one liner.
# tax_table2 <- 
#   tax_tbl %>%
#   dplyr::group_by(fy_year) %>%
#   dplyr::mutate(
#     tax_at = cumsum(data.table::shift(marginal_rate, type = "lag", fill = 0) * (lower_bracket - data.table::shift(lower_bracket, type = "lag", fill = 0))),
#     income = lower_bracket) %>%
#   .[, .(fy_year, income, lower_bracket, marginal_rate, tax_at)] %>%
#   data.table::as.data.table(.) %>%
#   data.table::setkey(fy_year, income)  # SETLENGHT ISSUE

shift0 <- function(x) shift(x, fill = 0)
tax_table2 <- 
  tax_tbl %>%
  copy %>%
  .[, tax_at := cumsum(shift0(marginal_rate) * (lower_bracket - shift0(lower_bracket))), by = "fy_year"] %>%
  .[, .(fy_year, income = lower_bracket, lower_bracket, marginal_rate, tax_at)] %>%
  setkey(fy_year, income)

lito_tbl <- 
  readxl::read_excel("./data-raw/lito-info.xlsx", sheet = 1) %>% 
  dplyr::select(-source) %>%
  as.data.table %>%
  setkeyv("fy_year") %>%
  unique(by = key(.))

lito_tbl %>% 
  readr::write_tsv("./data-raw/lito-info.tsv")

medicare_tbl_indiv <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.)

medicare_tbl <- 
  medicare_tbl_indiv %>%
  .[, sato := as.logical(sato)] %>%
  .[, pto := as.logical(pto)] %>%
  .[, sapto := as.logical(sapto)] %>%
  data.table::setkey(fy_year, sapto) %>%
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
  tryCatch({
    url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10.Q/ABS?startTime=1948"
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    as.data.frame(cpi) %>%
      as.data.table %>%
      .[, list(obsTime, obsValue)] %T>%
      fwrite("data-raw/cpi-unadjusted-manual.tsv", sep = "\t") %>%
      .[]
  },
  error = function(e) {
    fread("./data-raw/cpi-unadjusted-manual.tsv")
  })
  

cpi_seasonal_adjustment <-
  tryCatch({
    url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948"
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    as.data.frame(cpi) %>%
      as.data.table %>%
      .[, list(obsTime, obsValue)] %T>%
      fwrite("data-raw/cpi-seasonally-adjusted-manual.tsv", sep = "\t") %>%
      .[]
  },
  error = function(e) {
    fread("./data-raw/cpi-seasonally-adjusted-manual.tsv")
  })

cpi_trimmed <-
  tryCatch({
    url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948"
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    as.data.frame(cpi) %>%
      as.data.table %>%
      .[, list(obsTime, obsValue)] %T>%
      fwrite("data-raw/cpi-trimmed-mean-manual.tsv", sep = "\t") %>%
      .[]
  },
  error = function(e) {
    fread("./data-raw/cpi-seasonally-adjusted-manual.tsv")
  })

cpi_by_adj_fy <-
  list("none" = cpi_unadj, 
       "seasonal" = cpi_seasonal_adjustment,
       "trimmed.mean" = cpi_trimmed) %>%
  lapply(function(DT) {
    DT %>%
      .[endsWith(obsTime, "Q1")] %>%
      .[, .(obsValue),
        keyby = .(fy_year = yr2fy(as.integer(sub("-Q1", "", obsTime, fixed = TRUE))))]
  }) %>%
  rbindlist(idcol = "Adjustment", use.names = TRUE, fill = TRUE) %>%
  setkey(Adjustment, fy_year)

cpi_unadj_fy <- cpi_by_adj_fy[.("none"), .(fy_year, obsValue)]
cpi_seasonal_adjustment_fy <- cpi_by_adj_fy[.("seasonal"), .(fy_year, obsValue)]
cpi_trimmed_fy <- cpi_by_adj_fy[.("trimmed.mean"), .(fy_year, obsValue)]
setkey(cpi_unadj_fy, fy_year)
setkey(cpi_seasonal_adjustment_fy, fy_year)
setkey(cpi_trimmed_fy, fy_year)

min.cpi_unadj.yr <- fy2yr(cpi_unadj_fy[, first(fy_year)])
max.cpi_unadj.yr <- fy2yr(cpi_unadj_fy[, last(fy_year)])
min.cpi_seasonal_adjustment.yr <- fy2yr(cpi_seasonal_adjustment_fy[, first(fy_year)])
max.cpi_seasonal_adjustment.yr <- fy2yr(cpi_seasonal_adjustment_fy[, last(fy_year)])
min.cpi_trimmed.yr <- fy2yr(cpi_trimmed_fy[, first(fy_year)])
max.cpi_trimmed.yr <- fy2yr(cpi_trimmed_fy[, last(fy_year)])


wages_trend <- 
  tryCatch({
    wage.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/all?startTime=1997-Q3"
    wages <- rsdmx::readSDMX(wage.url)
    message("Using ABS sdmx connection")
    wage.indices <- 
      as.data.frame(wages) %>% 
      as.data.table %>%
      .[, list(obsTime, obsValue)]
    if (!{"2017-Q4" %in% .subset2(wage.indices, "obsTime")}) {
      wage.indices <- 
        rbind(wage.indices, 
              data.table(obsTime = "2017-Q4",
                         obsValue = 127.6))
    }
    fwrite(wage.indices, "./data-raw/wages-trend.tsv", sep = "\t")
    wage.indices
  },
  error = function(e) {
    data.table::fread("./data-raw/wages-trend.tsv", select = c("obsTime", "obsValue"))
  })

stopifnot(is.data.table(wages_trend))
split2yq <- function(x) {
  lapply(tstrsplit(x, split = ".Q", perl = TRUE),
         as.integer)
}
wages_trend[, c("obsYear", "obsQtr") := split2yq(obsTime)]
min.wage.yr <- wages_trend[, min(obsYear)]


`%fin%` <- fastmatch::`%fin%`

wages_trend_fy <- 
  wages_trend[obsQtr == 2L] %>%
  .[, .(fy_year = yr2fy(obsYear), 
                  obsValue)] %>%
  unique(by = "fy_year", fromLast = TRUE) %>%
  setkey(fy_year) %T>%
  # put match hash
  .[, stopifnot("2015-16" %fin% fy_year)] %>%
  .[]
max.wage.yr <- wages_trend_fy[, max(fy2yr(fy_year))]


lf_trend <- 
  tryCatch({
    lf.url.trend <- 
      # "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978-02"
      # "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978"
      "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/all?startTime=1978-02"
    lf <- rsdmx::readSDMX(lf.url.trend)
    lf <- 
      as.data.frame(lf) %>% 
      as.data.table %T>%
      {stopifnot(nrow(.) > 0)} %>%
      .[, .(obsTime, obsValue = as.integer(obsValue * 1000))]
    
    if ("2018-07" %notin% .subset2(lf, "obsTime") &&
        "2018-06" == lf[, last(obsTime)]) {
      message("Manually entering 2018-07 in lf")
      lf <- rbind(lf, 
                  data.table(obsTime = "2018-07",
                             obsValue = as.integer(13294.7 * 1000)))
    }
    
    fwrite(lf, "./data-raw/lf-trend.tsv", sep = "\t")
    lf
    
    
  }, 
  error = function(e){
    cat("Labour force retrieve errored: ", crayon::red(e$m), "\n")
    message("Using old (", as.character(file.mtime("./data-raw/lf-trend.tsv")), ") version of lf-trend.")
    data.table::fread("./data-raw/lf-trend.tsv" 
                      , select = c("obsTime", "obsValue"))
  })

lf_trend_fy <- 
  lf_trend[endsWith(obsTime, "-01")] %>%
  .[, .(fy_year = yr2fy(as.integer(substr(obsTime, 1L, 4L))), 
        obsValue)] %>%
  unique(by = "fy_year", fromLast = TRUE) %>%
  setkey(fy_year) %T>%
  # put match hash
  .[, stopifnot("2015-16" %fin% fy_year)] %>%
  .[]
min.lf.yr <- lf_trend_fy[, min(fy2yr(fy_year))]
max.lf.yr <- lf_trend_fy[, max(fy2yr(fy_year))]



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


MeanNumeric <- function(x){
  sum(as.numeric(x)) / length(x)
}

mean_of_nonzero <- function (x) {
  MeanNumeric(x[x > 0])
}

mean_of_each_taxstats_var <- 
  sample_files_all %>%
  .[, .SD, .SDcols = c("fy.year", names(.))] %>%
  select_which(is.numeric, .and.dots = "fy.year") %>%
  .[, lapply(.SD, MeanNumeric), by = "fy.year"]

meanPositive_of_each_taxstats_var <- 
  sample_files_all %>%
  .[, .SD, .SDcols = c("fy.year", names(.))] %>%
  select_which(is.numeric, .and.dots = "fy.year") %>%
  .[, lapply(.SD, mean_of_nonzero), by = "fy.year"]

round_if_num <- function(x, d = NULL) {
  if (is.double(x)) {
    if (is.null(d)) {
      if (all(data.table::between(x, 0, 1))) {
        round(x, 4)
      } else {
        round(x, 2)
      }
    } else {
      round(x, d)
    }
  } else {
    x
  }
}

# fy suffix
for (fys in c("1617")) {
  assign(paste0("generic_inflators_", fys), 
         value = local({
           if (!renew && file.exists(paste0("./data-raw/generic_inflators_", fys, ".tsv"))) {
             fread(file = paste0("./data-raw/generic_inflators_", fys, ".tsv"))
           } else {
             fy_long <- paste0("20", fys)
             lapply(1:15, 
                    function(h) {
                      grattan:::generic_inflator(vars = generic.cols,
                                                 h = h,
                                                 fy.year.of.sample.file = "2015-16") %>%
                        .[, H := h]
                    }) %>%
               rbindlist(use.names = TRUE) %>%
               .[, fy_year := yr2fy(2017L + H)] %>%
               .[, lapply(.SD, round_if_num)] %>%
               setnames("H", "h") %T>%
               write_tsv(paste0("./data-raw/generic_inflators_", fys, ".tsv")) %>%
               .[]
           }
         }))
}

generic_inflators_1516 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1516.tsv")
  } else {
    lapply(1:15, 
           function(h) {
             grattan:::generic_inflator(vars = generic.cols,
                                        h = h,
                                        fy.year.of.sample.file = "2015-16") %>%
               .[, H := h]
           }) %>%
      rbindlist(use.names = TRUE) %>%
      .[, fy_year := yr2fy(2016 + H)] %>%
      .[, lapply(.SD, round_if_num)] %>%
      setnames("H", "h") %T>%
      write_tsv("./data-raw/generic_inflators_1516.tsv") %>%
      .[]
  }

generic_inflators_1516 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1516.tsv")
  } else {
    lapply(1:15, 
           function(h) {
             grattan:::generic_inflator(vars = generic.cols,
                                        h = h,
                                        fy.year.of.sample.file = "2015-16") %>%
               .[, H := h]
           }) %>%
      rbindlist(use.names = TRUE) %>%
      .[, fy_year := yr2fy(2016 + H)] %>%
      .[, lapply(.SD, round_if_num)] %>%
      setnames("H", "h") %T>%
      write_tsv("./data-raw/generic_inflators_1516.tsv") %>%
      .[]
  }

generic_inflators_1415 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1415.tsv")
  } else {
    lapply(1:10, 
           function(h) {
             grattan:::generic_inflator(vars = generic.cols,
                                        h = h,
                                        fy.year.of.sample.file = "2014-15") %>%
             .[, H := h]
           }) %>%
      rbindlist(use.names = TRUE) %>%
      .[, fy_year := yr2fy(2015 + H)] %>%
      .[, lapply(.SD, round_if_num)] %>%
      setnames("H", "h") %T>%
      write_tsv("./data-raw/generic_inflators_1415.tsv") %>%
      .[]
  }


generic_inflators_1314 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1314.tsv")
  }  else {
    lapply(1:10, 
           function(h) {
             grattan:::generic_inflator(vars = generic.cols,
                                        h = h,
                                        fy.year.of.sample.file = "2013-14") %>%
               .[, H := h]
           }) %>%
      rbindlist(use.names = TRUE) %>%
      .[, fy_year := yr2fy(2014 + H)] %>%
      .[, lapply(.SD, round_if_num)] %>%
      setnames("H", "h") %T>%
      write_tsv("./data-raw/generic_inflators_1314.tsv") %>%
      .[]
    
  }

generic_inflators_1213 <- 
  if (!renew){
    fread("./data-raw/generic_inflators_1213.tsv")
  } else {
    lapply(1:10, 
           function(h) {
             grattan:::generic_inflator(vars = generic.cols,
                                        h = h,
                                        fy.year.of.sample.file = "2012-13") %>%
               .[, H := h] 
           }) %>%
      rbindlist(use.names = TRUE) %>%
      .[, fy_year := yr2fy(2013 + H)] %>%
      .[, lapply(.SD, round_if_num)] %>%
      setnames("H", "h") %T>%
      write_tsv("./data-raw/generic_inflators_1213.tsv") %>%
      .[]
  }

cg_inflators_1415 <- if (!renew) fread("./data-raw/cg_inflators_1415.tsv") else {
  get_cg_inf <- function(series = "mean") {
    
    cg_table <- 
      sample_files_all %>%
      .[Net_CG_amt > 0, .(fy.year, Taxable_Income, Net_CG_amt)] %>%
      .[, marginal_rate_first := income_tax(Taxable_Income + 1, 
                                            fy.year = fy.year) - income_tax(Taxable_Income, 
                                                                            fy.year = fy.year)] %>%
      .[, marginal_rate_last := (income_tax(Taxable_Income + Net_CG_amt, fy.year = fy.year) - income_tax(Taxable_Income, fy.year = fy.year)) / Net_CG_amt] %>%
      .[, .(mean_mr1 = mean(marginal_rate_first), 
            mean_wmr1 = stats::weighted.mean(marginal_rate_first, Net_CG_amt), 
            mean_mrL = mean(marginal_rate_last), 
            mean_wmrL = stats::weighted.mean(marginal_rate_last, Net_CG_amt)),
        keyby = 'fy.year'] %>%
      .[cgt_expenditures, on = "fy.year==FY"] %>%
      drop_cols(c("URL", "Projected")) %>%
      setkey(fy.year) %>%
      setnames("CGT_discount_for_individuals_and_trusts_millions", "revenue_foregone") %>%
      .[, .(fy.year,
            zero_discount_Net_CG_total = revenue_foregone * 10^6 / mean(mean_wmrL, na.rm = TRUE))]
    
    
    if (!exists("individuals_table1_201415")) {
      individuals_table1_201415 <- 
        if (file.exists("~/taxstats/data-raw/2014-15/individuals_table1_201415.tsv")) {
          fread("~/taxstats/data-raw/2014-15/individuals_table1_201415.tsv")
        } else {
          stop("individuals_table1_201415 does not exist.")
        }
    }
    
    n_cg_history <- 
      as.data.table(individuals_table1_201415) %>%
      .[Selected_items == "Net capital gain"] %>%
      .[!is.na(Count)] %>%
      .[, .(fy_year, n_CG = Count)]
    
    forecaster <- function(object, ...) {
      forecast(object, ...)
    } # gforecast terrible
    
    
    
    switch(series,
           "mean" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$mean))))
             
             cg <- 
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$mean)))
           }, 
           "lower" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$lower))))
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$lower)))
           }, 
           "upper" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:10),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$upper))))
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:10), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$upper)))
           })
    
    cg_inf <- n_cg[cg, on = "fy_year==fy.year", nomatch=0L]
    
    cg_inf[, cg_inflator := zero_discount_Net_CG_total / n_CG]
    cg_inf[, cg_inflator := cg_inflator / cg_inflator[cg_inf$fy_year == "2014-15"]]
    cg_inf[, forecast.series := series]
  }
  lapply(c("lower", "mean", "upper"), get_cg_inf) %>%
    rbindlist(use.names = TRUE) %>%
    .[fy_year >= "2012-13"] %T>%
    write_tsv("./data-raw/cg_inflators_1415.tsv") %>%
    .[]
}

cg_inflators_1516 <- if (!renew) fread("./data-raw/cg_inflators_1516.tsv") else {
  get_cg_inf <- function(series = "mean") {
    marginal_rate <- function(tx, fy) {
      income_tax(tx + 1, fy.year = fy) - income_tax(tx, fy.year = fy)
    }
    
    
    cg_table <- 
      sample_files_all %>%
      .[Net_CG_amt > 0, .(fy.year, Taxable_Income, Net_CG_amt)] %>%
      .[, marginal_rate_first := marginal_rate(Taxable_Income, fy.year)] %>%
      .[, marginal_rate_last := (income_tax(Taxable_Income + Net_CG_amt, fy.year = fy.year) - income_tax(Taxable_Income, fy.year = fy.year)) / Net_CG_amt] %>%
      .[, .(mean_mr1 = mean(marginal_rate_first), 
            mean_wmr1 = stats::weighted.mean(marginal_rate_first, Net_CG_amt), 
            mean_mrL = mean(marginal_rate_last), 
            mean_wmrL = stats::weighted.mean(marginal_rate_last, Net_CG_amt)),
        keyby = 'fy.year'] %>%
      .[cgt_expenditures, on = "fy.year==FY"] %>%
      drop_cols(c("URL", "Projected")) %>%
      setkey(fy.year) %>%
      setnames("CGT_discount_for_individuals_and_trusts_millions", "revenue_foregone") %>%
      .[, .(fy.year,
            zero_discount_Net_CG_total = revenue_foregone * 10^6 / mean(mean_wmrL, na.rm = TRUE))]
    
    
      if (!exists("individuals_table1_201516")) {
        individuals_table1_201516 <- 
          if (file.exists("~/taxstats/data-raw/2015-16/individuals_table1_201516.tsv")) {
            fread("~/taxstats/data-raw/2015-16/individuals_table1_201516.tsv", na.strings = c("", "NA", "f"))
          } else {
            stop("individuals_table1_201516 does not exist.")
          }
      }
    
    n_cg_history <- 
      as.data.table(individuals_table1_201516) %>%
      .[Selected_items == "Net capital gain"] %>%
      .[!is.na(Count)] %>%
      .[, .(fy_year, n_CG = Count)]
    
    forecaster <- function(object, ...) {
      forecast(object, h = 15, ...)
    } # gforecast terrible
    
    
    
    switch(series,
           "mean" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:15),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$mean))))
             
             cg <- 
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:15), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$mean)))
           }, 
           "lower" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:15),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$lower))))
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:15), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$lower)))
           }, 
           "upper" = {
             n_cg <- 
               rbind(n_cg_history, 
                     data.table(fy_year = yr2fy(fy2yr(last(n_cg_history$fy_year)) + 1:15),
                                n_CG = exp(as.numeric(forecaster(log(n_cg_history$n_CG), level = 95)$upper))))
             
             cg <-
               rbind(as.data.table(cg_table), 
                     data.table(fy.year = yr2fy(fy2yr(last(cg_table$fy.year)) + 1:15), 
                                zero_discount_Net_CG_total = as.numeric(forecaster(cg_table$zero_discount_Net_CG_total, level = 95)$upper)))
           })
    
    cg_inf <- n_cg[cg, on = "fy_year==fy.year", nomatch=0L]
    
    cg_inf[, cg_inflator := zero_discount_Net_CG_total / n_CG]
    cg_inf[, cg_inflator := cg_inflator / cg_inflator[cg_inf$fy_year == "2015-16"]]
    cg_inf[, forecast.series := series]
  }
  lapply(c("lower", "mean", "upper"), get_cg_inf) %>%
    rbindlist(use.names = TRUE) %>%
    .[fy_year >= "2012-13"] %T>%  # leave as 2012-13 to keep earlier years working
    write_tsv("./data-raw/cg_inflators_1516.tsv") %>%
    .[]
}

cg_inflators_1314 <- 
  cg_inflators_1415 %>%
  copy %>%
  .[, cg_inflator := cg_inflator / cg_inflator[cg_inflators_1415$fy_year == "2013-14"]] %>%
  .[]

cg_inflators_1213 <-
  cg_inflators_1415 %>%
  copy %>%
  .[, cg_inflator := cg_inflator / cg_inflator[cg_inflators_1415$fy_year == "2012-13"]] %>%
  .[]
  
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
  .[Sw_amt > 0, .(fy.year, Sw_amt)] %>%
  .[, Sw_amt_percentile := ntile(Sw_amt, 100), keyby = "fy.year"] %>%
  .[,
    .(average_salary = mean(Sw_amt), 
      min_salary = min(Sw_amt)),
    keyby = .(fy.year, Sw_amt_percentile)] %>%
  .[, average_salary := round(average_salary, 2L)] %>%
  setkey(Sw_amt_percentile) %>%
  .[]

if (salary_by_fy_swtile[, max(fy.year)] < "2015-16") {
  warning("salary_by_fy_swtile not available.")
  rm(salary_by_fy_swtile)
  salary_by_fy_swtile <- grattan:::salary_by_fy_swtile
}

  

differential_sw_uprates <- 
  salary_by_fy_swtile %>%
  copy %>%
  setkey(Sw_amt_percentile, fy.year) %>%
  .[, r_average_salary := average_salary / shift(average_salary) - 1, keyby = "Sw_amt_percentile"] %>%
  .[fy.year != min(fy.year)] %>%
  .[, .(avg_r = mean(r_average_salary)), keyby = "Sw_amt_percentile"] %>%
  .[, .(Sw_amt_percentile, uprate_factor_raw = avg_r / mean(avg_r))] %>%
  setkey(Sw_amt_percentile) %>%
  # Span = 0.5 seems to be the point at which the curve has only 
  # one local extremum.
  # ggplot(., aes(x = Sw_amt_percentile, y = uprate_factor)) + geom_point() + stat_smooth(method = "loess", span = 0.45)
  .[, uprate_factor := predict(loess(uprate_factor_raw ~ Sw_amt_percentile, data = ., span = 0.40), newdata = .)] %>%
  .[]

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
  if (file.exists("data-raw/Age-Pension-assets-test-1997-present.tsv")) {
    Age_pension_assets_test_by_year <- 
      fread("data-raw/Age-Pension-assets-test-1997-present.tsv", logical01 = TRUE) %>%
      .[, Date := as.Date(Date)] %>%
      setkeyv(c("HasPartner", "IllnessSeparated", "HomeOwner", "Date")) %>%
      .[]
    if (Age_pension_assets_test_by_year[, difftime(Sys.Date(), max(Date), units = "days")] > 400) {
      warning("`Age_pension_assets_test_by_year` out-of-date by some 400 days.")
    }
    Age_pension_assets_test_by_year
    
  } else {
    age_pension_assets_test_dss_gov_au <- 
      htmltab::htmltab("http://guides.dss.gov.au/guide-social-security-law/4/10/3", which = 10)
    stopifnot(identical(names(age_pension_assets_test_dss_gov_au),
                        c("Date",
                          "Homeowners >> Single",
                          "Homeowners >> Couple",
                          "Homeowners >> Illness Separated Couple", 
                          "Non-homeowners >> Single",
                          "Non-homeowners >> Couple",
                          "Non-homeowners >> Illness Separated Couple", 
                          "Notes")))
    as.data.table(age_pension_assets_test_dss_gov_au) %>%
      .[, Notes := NULL] %>%
      melt.data.table(id.vars = "Date", 
                      variable.factor = FALSE) %>%
      .[, c("t1", "t2") := tstrsplit(variable, split = " >> ", fixed = TRUE)] %>%
      .[, variable := NULL] %>%
      .[, Date := as.Date(Date, format = "%d/%m/%Y")] %>%
      .[, assets_test := as.integer(gsub(",", "", value))] %>%
      .[, .(HasPartner = endsWith(t2, "Couple"),
            IllnessSeparated = startsWith(t2, "Illness Separated"),
            HomeOwner = startsWith(t1, "Homeowner"),
            Date,
            assets_test)] %>%
      setkeyv(c("HasPartner", "IllnessSeparated", "HomeOwner", "Date")) %T>%
      .[, stopifnot(has_unique_key(.SD))] %>%
      # .[, .(Date, t1, t2, assets_test)] %>%
      # .[]
      .[, Date := as.character(Date)] %T>%
      fwrite("data-raw/Age-Pension-assets-test-1997-present.tsv",
             sep = "\t",
             logical01 = TRUE) %>%
      .[, Date := as.Date(Date)] %>%
      setkeyv(c("HasPartner", "IllnessSeparated", "HomeOwner", "Date")) %>%
      .[]
    
    
  }

bto_tbl <- 
  read_excel("data-raw/beneficiary-tax-offset-by-fy.xlsx") %>%
  as.data.table %>%
  setkey(fy_year)

fwrite(bto_tbl, "data-raw/bto_tbl.tsv", sep = "\t")

Age_pension_permissible_income_by_Date <- 
  read_excel("data-raw/age-pension-permissible-income.xlsx") %>%
  gather(type, permissible_income, -Date) %>%
  mutate(type = trimws(gsub("Permissible income ", "", gsub("[^A-Za-z]", " ", type)))) %>%
  as.data.table

Age_pension_deeming_rates_by_Date <-
  # http://guides.dss.gov.au/guide-social-security-law/4/4/1/10
  read_excel("data-raw/Age-Pension-deeming-rates-1996-2016.xlsx") %>%
  setDT %>%
  melt.data.table(id.vars = c("Date", "deeming_rate_below", "deeming_rate_above"),
                  variable.name = "type", value.name = "threshold") %>%
  .[, Date := as.Date(Date)] %>%
  .[, type := gsub("_", " ", gsub("threshold_", "", type, fixed = TRUE))] %>%
  .[, .(Date, type, threshold, deeming_rate_below, deeming_rate_above)]

.aus_pop_by_age_yearqtr <-
  rsdmx::readSDMX(paste0("http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.0",
                         paste0(c("", 1:100), collapse = "+"),
                         ".Q/all?startTime=1981-Q3")) %>%
  as.data.frame %>%
  as.data.table 

if ("AGE" %in% names(.aus_pop_by_age_yearqtr)) {
  setnames(.aus_pop_by_age_yearqtr, "AGE", "Age")
}

if (nrow(.aus_pop_by_age_yearqtr) > 14e3L) {
  fwrite(drop_constant_cols(.aus_pop_by_age_yearqtr), 
         file = "./data-raw/Estim-Resi-Pop-by-age-1981-present.csv")
}

.aus_pop_by_yearqtr <-
  rsdmx::readSDMX("http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.TT.Q/all?startTime=1981-Q3&endTime=2017-Q3") %>%
  as.data.frame %>%
  as.data.table



if (nrow(.aus_pop_by_yearqtr) > 145) {
  fwrite(drop_constant_cols(.aus_pop_by_yearqtr), 
         file = "./data-raw/Estim-Res-Pop-1981-present.tsv",
         sep = "\t")
}

austres_reshaped <- 
  data.table(obsTime = paste0(floor(1971 + seq_along(austres) / 4),
                              "-Q", 1 + seq_along(austres) %% 4),
             obsValue = as.integer(1000 * as.numeric(austres))) %>%
  .[obsTime < "1981-Q3"]

aus_pop_by_yearqtr <- 
  rbind(austres_reshaped, 
        fread("./data-raw/Estim-Res-Pop-1981-present.tsv")) %>%
  setkey(obsTime)

aust_pop_by_age_yearqtr <- 
  fread("./data-raw/Estim-Resi-Pop-by-age-1981-present.csv") %>%
  .[, Q := as.integer(substr(obsTime, 7, 7))] %>%
  .[, Y := substr(obsTime, 0, 4)] %>%
  .[, date_char := paste0("01-", month.name[(Q)*3], "-", Y)] %>%
  .[, .(Age, 
        Date = as.Date(date_char, format = "%d-%B-%Y"),
        Value = obsValue)] %>%
  setkey(Age, Date)

abs_key_aggregates_url_status <- 
  download.file("http://www.ausstats.abs.gov.au/ausstats/meisubs.nsf/LatestTimeSeries/5206001_key_aggregates/$FILE/5206001_key_aggregates.xls",
                mode = "wb",
                destfile = "data-raw/5206001_key_aggregates-latest-release.xls")

if (!abs_key_aggregates_url_status) {
# abs_key_aggregates_names <- 
#   read_excel("data-raw/5206001_key_aggregates-latest-release.xls",
#              sheet = "Data1",
#              range = cell_rows(2:10)) %>%
#   dplyr::filter(Unit == "Series ID")
# 
# units_by_series_id <- 
#   data.table(Series_ID = as.character(abs_key_aggregates_names), 
#              units = names(abs_key_aggregates_names)) %>%
#   .[Series_ID != "Series ID"] %>%  # 'Series ID' and 'units'
#   .[, units := sub("__.*$", "", units, perl = TRUE)] %>%


abs_key_aggregates_metadata <-
  read_excel("data-raw/5206001_key_aggregates-latest-release.xls", sheet = "Index", skip = 10) %>%
  as.data.table %>%
  .[, list(Description = `Data Item Description`,
           Series_ID = `Series ID`,
           Series_type = `Series Type`,
           Unit,
           Freq = Freq.)] %>%
  .[!is.na(Series_ID)] %>%
  .[, c("Description", "PostColon") := tstrsplit(Description, split = ":", fixed = TRUE)] %>%
  .[!grepl("Commonwealth of Australia", Description, fixed = TRUE)] %>%
  .[, multiply := if_else(grepl("million", Unit, ignore.case = TRUE),
                          10^6,
                          if_else(grepl("(percent)|(proportion)", Unit, ignore.case = TRUE),
                                  0.01,
                                  1))] %>%
  .[]

abs_key_aggregates <- 
  read_excel("data-raw/5206001_key_aggregates-latest-release.xls", sheet = "Data1", skip = 9) %>%
  as.data.table %>%
  melt.data.table(id.vars = "Series ID",
                  variable.name = "Series_ID") %>%
  setnames("Series ID", "Date") %>%
  merge(abs_key_aggregates_metadata, by = "Series_ID", sort = FALSE) %>%
  .[, list(Series_ID,
           # Description,
           Date = as.Date(Date),
           value = value * multiply)] %>%
  # to limit size to only what is used
  # .[Series_ID %chin% c("A2304350J"   # GDP
  #                      ,"A2304354T"  # GNI
  #                      )]
  .[]

fwrite(abs_key_aggregates, "data-raw/abs_key_aggregates.tsv", sep = "\t")
} else {
  abs_key_aggregates <- fread("data-raw/abs_key_aggregates.tsv")
  if (as.double(difftime(Sys.Date(),
                         as.Date(last(abs_key_aggregates[["Date"]])),
                         units = "days")) > 182) {
    stop("ABS key aggregates too old.\nLast date:\t", last(abs_key_aggregates[["Date"]]))
  }
  
}

read_csv_2col <- function(...) {
  if (httr::http_error(..1)) {
    message("ausmacrodata.org unavbl")
    return(NULL)
  }
  suppressMessages(suppressWarnings(read_csv(...))) %>% select(1:2) %>% setDT
}

if (FALSE) {
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
  melt.data.table(id.vars = "Date",
                  variable.name = "City",
                  value.name = "Residential_property_price_index")
}

residential_property_prices <- 
"http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/RES_PROP_INDEX/1.3.1GSYD+2GMEL+3GBRI+4GADE+5GPER+6GHOB+7GDAR+8ACTE+100.Q/all?startTime=2002-Q1&endTime=2018-Q2" %>% 
  readSDMX %>%
  as.data.frame %>% 
  setDT %>%
  .[ASGS_2011 %ein% "3GBRI", City := "Brisbane"] %>%
  .[ASGS_2011 %ein% "5GPER", City := "Perth"] %>%
  .[ASGS_2011 %ein% "7GDAR", City := "Darwin"] %>%
  .[ASGS_2011 %ein% "6GHOB", City := "Hobart"] %>%
  .[ASGS_2011 %ein% "8ACTE", City := "Canberra"] %>%
  .[ASGS_2011 %ein% "1GSYD", City := "Sydney"] %>%
  .[ASGS_2011 %ein% "4GADE", City := "Adelaide"] %>%
  .[ASGS_2011 %ein% "100", City := "Australia (weighted average)"] %>%
  .[ASGS_2011 %ein% "2GMEL", City := "Melbourne"] %>%
  .[, City := factor(City, 
                     levels = c("Sydney", "Melbourne", "Brisbane",
                                "Perth", "Adelaide", "Hobart", 
                                "Canberra", "Darwin",
                                "Australia (weighted average)"))] %>%
  .[, Date := as.Date(paste0(substr(obsTime, 1, 4), 
                             "-",
                             3L * as.integer(substr(obsTime, 7, 7)),
                             "-01"))] %>%
  .[, .(Date, City, Residential_property_price_index = obsValue)] %>%
  setkey(Date, City) %>%
  .[]



usethis::use_data(residential_property_prices, overwrite = TRUE)
fwrite(residential_property_prices,
       "data-raw/sysdata/residential_property_prices.tsv", 
       sep = "\t",
       logical01 = TRUE)

NewstartRatesTable.raw <-
  "http://guides.dss.gov.au/guide-social-security-law/5/2/1/20" %>%
  htmltab::htmltab(which = '//*[@id="node-16056"]/table[11]')

NewstartRatesOver21Single1991 <-
  "http://guides.dss.gov.au/guide-social-security-law/5/2/1/20" %>%
  htmltab::htmltab(which = '//*[@id="node-16056"]/table[9]')

newstart_rates_table <-
  NewstartRatesOver21Single1991 %>%
  as.data.table %>%
  .[, is_single := TRUE] %>%
  .[, min_age := 21] %>%
  .[, max_age := Inf] %>%
  # Only include entries following the heading within the table
  .[seq_len(.N) > which(Date %pin% "From 1 July 1991.*became")] %>%
  .[, Rate := sub(" Note [MN]", "", Rate, perl = TRUE)] %>%
  .[, Date := as.Date(Date, format = "%d/%m/%Y")] %>%
  .[, Rate := as.numeric(Rate)] %T>%
  fwrite("data-raw/Newstart-allowance-rates.tsv", sep = "\t") %>%
  .[] 

CPI_July2015 <- function(income_free_area, Date) {
  {
    100L * cpi_inflator_general_date(from_date = "2015-07-01",
                                     to_date = Date)
  } %>%
    ceiling %>%
    as.integer
}

newstart_income_test <-
  # http://guides.dss.gov.au/guide-social-security-law/4/10/2
  fread(input = "
Date  income_free_area  income_threshold
2015-07-01  102 252
2014-03-20  100 250
2013-01-01  62  250
2006-07-01  62  250
2000-07-01  60  140
") %>%
  .[, Date := as.Date(Date)] %>%
  setkey(Date) %>%
  .[newstart_rates_table, roll = TRUE] %>%
  .[Date > "2015-07-01", 
    income_free_area := CPI_July2015(income_free_area, Date)] %>%
  .[complete.cases(.)] %>%  # avoid inflating prematurely
  .[, max_income_allowed := (10 / 4) * (Rate - 0.5 * (income_threshold - income_free_area))] %>%
  .[] %T>%
  fwrite("data-raw/newstart-income-test.tsv", sep = "\t") %>% 
  .[]

source("./data-raw/CAPITA/extract_clean_capita.R")

forecast_with_orig <- function(x) {
  y <- ts(x, freq = 2)
  the_forecast <- forecast::forecast(y, h = 10L)
  ans <- 
    c(the_forecast[["x"]], 
      the_forecast[["mean"]])
  if (is.integer(x)) {
    as.integer(ans)
  } else {
    ans
  }
}

parenting_payment_by_date <- 
  htmltab::htmltab("data-raw/guides.dss.gov.au/guide-social-security-law/5/2/4/50",
                   which = 3) %>%
  as.data.table %>%
  setnames("Date rate commenced", "Date") %>%
  setnames("Pension PPS", "MBR") %>% # for consistency with CAPITA tables
  .[, MBR := sub(" Note A", "", MBR, fixed = TRUE)] %>%
  .[, MBR := as.double(MBR)] %T>%
  .[, stopifnot(!anyNA(MBR))] %>%
  .[, Date := as.Date(Date, format = "%d/%m/%Y")] %>%
  setkey(Date) %>%
  .[, Year := year(Date)] %>%
  .[, Month := month(Date)] %>%
  .[, lapply(.SD, forecast_with_orig), .SDcols = c("Year", "Month", "MBR")] %>%
  .[, Date := as.Date(paste0(Year, "-", Month, "-20"))] %>%
  .[, MBR := round(MBR, 2)] %>%
  setkey(Date) %>%
  .[]

parenting_payment_by_fy <- 
  parenting_payment_by_date %>%
  .[, .(MBR = mean(MBR)),
    keyby = .(fy_year = date2fy(Date))]

parenting_payment_by_fy <- 
  rbind(copy(parenting_payment_by_fy)[, isParentingPayment := FALSE][, MBR := NA_real_],
        copy(parenting_payment_by_fy)[, isParentingPayment := TRUE]) %>%
  setkeyv(c("isParentingPayment", "fy_year"))

# http://guides.dss.gov.au/guide-social-security-law/4/2/2
.partner_income_free_area <- function(fy.year,
                                      partner_receives_benefit,
                                      age) {
  input <- CJ(fy_year = fy.year,
              PartnerReceivesBenefit = partner_receives_benefit,
              Age = age)
  input[, II := .I]
  input[
    ,
    "partner_income_free_area" := if (length(Age) != 1L || length(fy_year) != 1L) {
      stop("Unexpected number of ages.")
    } else if (Age < 22) {
      which.max(youth_allowance(1:5e3, fy.year = fy_year, is_student = FALSE, per = "fortnight") < 0.01) + 1
    } else if (Age < 65) {
      which.max(unemployment_benefit(1:5e3, fy.year = fy_year) < 0.01) + 1
    } else {
      which.max(unemployment_benefit(1:5e3, fy.year = fy_year) < 0.01) + 1
    },
    keyby = "II"]
  input[, II := NULL]
  setkeyv(input, c("fy_year", "PartnerReceivesBenefit", "Age"))
  input[]
}

partner_income_free_area_by_fy_student_age <- 
  tryCatch(getFromNamespace("partner_income_free_area_by_fy_student_age",
                            ns = asNamespace("grattan")),
           error = function(e) {
             .partner_income_free_area(yr2fy(2005:2020), c(FALSE, TRUE), c(0L, 22L, 65L))
  })

if (hasName(partner_income_free_area_by_fy_student_age, 
            "PartnerReceivesBenefit")) {
  setnames(partner_income_free_area_by_fy_student_age, 
           "PartnerReceivesBenefit",
           "partnerIsPensioner")
}

transpose2 <- function(DT, new.names.j = 1L) {
  new_names <- DT[[new.names.j]]
  old_names <- names(DT)
  out <- t(DT)[-1, ]
  out <- as.data.table(out)
  setnames(out, 1L, "orig")
  setnames(out, names(out), new_names)
  out
}


AWOTE.xls <- tempfile("AWOTE-May2018", fileext = ".xls")
"http://www.ausstats.abs.gov.au/ausstats/meisubs.nsf/0/3DFE8BCE606106CDCA2582EA00193BAB/$File/6302001.xls" %>%
  download.file(destfile = AWOTE.xls, 
                mode = "wb", 
                quiet = TRUE)

AWOTE_Header <-
  read_excel(AWOTE.xls,
             sheet = "Data1",
             n_max = 10,
             col_names = FALSE) %>%
  setDT %>%
  setnames(1L, "id") %>%
  .[1L, id := coalesce(id, "Description")] %>%
  transpose2 %>%
  .[, Sex := if_else(grepl("Persons", Description),
                     "Persons",
                     if_else(grepl("Males", Description),
                             "Male",
                             "Female"))] %>%
  .[, Earnings := if_else(grepl("Total", Description),
                          "Total",
                          "Ordinary")] %>%
  .[, isAdult := grepl("Adult", Description)] %>%
  .[]
  
AWOTE_Data <-
  read_excel(AWOTE.xls, "Data1", skip = 9) %>%
  as.data.table %>%
  setnames("Series ID", "Date") %>%
  melt.data.table(id.vars = "Date",
                  variable.name = "Series ID",
                  variable.factor = FALSE,
                  value.name = "AWOTE") %>%
  .[AWOTE_Header[, .SD, .SDcols = c("Series ID", "Sex", "Earnings", "isAdult")],
    on = c("Series ID"),
    nomatch=0L] %>%
  .[, Date := as.character(Date)] %>%
  .[]

AWOTE1994_2011.xls <- tempfile(fileext = ".xls")
download.file("http://www.ausstats.abs.gov.au/ausstats/meisubs.nsf/0/8F061DFDA1200856CA2579AC000D5F10/$File/6302001.xls", 
              mode = "wb", 
              destfile = AWOTE1994_2011.xls)

update_SeriesExcelDates <- function(DT) {
  for (j in grep("Series (Start|End)", names(DT))) {
    set(DT, j = j, value = as.Date(as.integer(DT[[j]]), origin = "1899-12-30"))
  }
  DT
}

AWOTE1994_2011_Header <-
  read_excel(AWOTE1994_2011.xls,
             sheet = "Data1",
             n_max = 10,
             col_names = FALSE) %>%
  setDT %>%
  setnames(1L, "id") %>% 
  .[1L, id := coalesce(id, "Description")] %>%
  transpose2 %>%
  update_SeriesExcelDates %>%
  .[, Sex := if_else(grepl("Persons", Description),
                     "Persons",
                     if_else(grepl("Males", Description),
                             "Male",
                             "Female"))] %>%
  .[, Earnings := if_else(grepl("Total", Description),
                          "Total",
                          "Ordinary")] %>%
  .[, isAdult := grepl("Adult", Description)] %>%
  .[]

AWOTE1994_2011_Data <- 
  read_excel(AWOTE1994_2011.xls, "Data1", skip = 9) %>%
  as.data.table %>% 
  setnames("Series ID", "Date") %>%
  melt.data.table(id.vars = "Date",
                  variable.name = "Series ID",
                  variable.factor = FALSE,
                  value.name = "AWOTE") %>%
  .[AWOTE1994_2011_Header[, .SD, .SDcols = c("Series ID", "Sex", "Earnings", "isAdult")],
    on = c("Series ID"),
    nomatch=0L] %>%
  .[, Date := as.character(Date)] %>%
  .[]

AWOTE_by_Date_isMale_isOrdinary_isAdult <- 
  CJ(Date = seq.Date(as.Date("1986-02-15"), 
                     as.Date("1994-05-15"), 
                     by = "3 months"),
     Sex = c("Male", "Female", "Persons"),
     Earnings = c("Ordinary", "Total"),
     isAdult = FALSE) %>%
  .[, "Series ID" := NA_character_] %>%
  .[, Date := as.character(Date)] %>%
  setkey(Date, Sex, Earnings, isAdult) %>%
  # http://www.ausstats.abs.gov.au/ausstats/free.nsf/0/B0E788EAD7591E6ECA2574FF001962A2/$File/63020_FEB1986.pdf
  .[.("1985-02-15", "Male", "Ordinary"), AWOTE := 399.60] %>%
  .[.("1985-05-17", "Male", "Ordinary"), AWOTE := 404.50] %>%
  .[.("1985-08-16", "Male", "Ordinary"), AWOTE := 409.80] %>%
  .[.("1985-11-15", "Male", "Ordinary"), AWOTE := 419.60] %>%
  
  .[.("1985-02-15", "Male", "Total"), AWOTE := 392.70] %>%
  .[.("1985-05-17", "Male", "Total"), AWOTE := 397.20] %>%
  .[.("1985-08-16", "Male", "Total"), AWOTE := 403.10] %>%
  .[.("1985-11-15", "Male", "Total"), AWOTE := 413.90] %>%
  
  .[.("1985-02-15", "Female", "Ordinary"), AWOTE := 328.40] %>%
  .[.("1985-05-17", "Female", "Ordinary"), AWOTE := 334.40] %>%
  .[.("1985-08-16", "Female", "Ordinary"), AWOTE := 338.70] %>%
  .[.("1985-11-15", "Female", "Ordinary"), AWOTE := 345.30] %>%
  
  .[.("1985-02-15", "Female", "Total"), AWOTE := 260.10] %>%
  .[.("1985-05-17", "Female", "Total"), AWOTE := 263.40] %>%
  .[.("1985-08-16", "Female", "Total"), AWOTE := 265.00] %>%
  .[.("1985-11-15", "Female", "Total"), AWOTE := 268.40] %>%
  
  
  .[.("1985-02-15", "Persons", "Ordinary"), AWOTE := 377.50] %>%
  .[.("1985-02-15", "Persons", "Total"), AWOTE := 340.10] %>%
  
  .[.("1986-02-15", "Male", "Ordinary"), AWOTE := 427.20] %>%
  .[.("1986-02-15", "Male", "Total"), AWOTE := 422.7] %>%
  .[.("1986-02-15", "Female", "Ordinary"), AWOTE := 352.8] %>%
  .[.("1986-02-15", "Female", "Total"), AWOTE := 276.4] %>%
  .[.("1986-02-15", "Persons", "Ordinary"), AWOTE := 404.20] %>%
  .[.("1986-02-15", "Persons", "Total"), AWOTE := 364.10] %>%
  
  # http://www.ausstats.abs.gov.au/ausstats/free.nsf/0/B0E788EAD7591E6ECA2574FF001962A2/$File/63020_FEB1986.pdf
  .[.("1985-02-15", "Male", "Ordinary"), AWOTE := 399.60] %>%
  .[.("1985-02-15", "Male", "Total"), AWOTE := 429] %>%
  .[.("1985-02-15", "Female", "Ordinary"), AWOTE := 328.4] %>%
  .[.("1985-02-15", "Female", "Total"), AWOTE := 335.9] %>%
  .[.("1985-02-15", "Persons", "Ordinary"), AWOTE := 377.50] %>%
  .[.("1985-02-15", "Persons", "Total"), AWOTE := 400.10] %>%
  
  ## 1992
  .[.("1992-05-15", "Male", "Ordinary"), AWOTE := 625.20] %>%
  .[.("1992-08-21", "Male", "Ordinary"), AWOTE := 625.10] %>%
  .[.("1992-11-20", "Male", "Ordinary"), AWOTE := 624.40] %>%
  
  .[.("1992-05-15", "Male", "Total"), AWOTE := 598.80] %>%
  .[.("1992-08-21", "Male", "Total"), AWOTE := 599.50] %>%
  .[.("1992-11-20", "Male", "Total"), AWOTE := 601.20] %>%

  .[.("1992-05-15", "Female", "Ordinary"), AWOTE := 520.70] %>%
  .[.("1992-08-21", "Female", "Ordinary"), AWOTE := 520.70] %>%
  .[.("1992-11-20", "Female", "Ordinary"), AWOTE := 522.50] %>%
  
  .[.("1992-05-15", "Female", "Total"), AWOTE := 398.20] %>%
  .[.("1992-08-21", "Female", "Total"), AWOTE := 398.50] %>%
  .[.("1992-11-20", "Female", "Total"), AWOTE := 399.80] %>%
  
  .[.("1992-05-15", "Persons", "Ordinary"), AWOTE := 587.90] %>%
  .[.("1992-08-21", "Persons", "Ordinary"), AWOTE := 587.60] %>%
  .[.("1992-11-20", "Persons", "Ordinary"), AWOTE := 587.90] %>%
  
  ## 1993
  .[.("1993-02-19", "Male", "Ordinary"), AWOTE := 627.50] %>%
  .[.("1993-05-21", "Male", "Ordinary"), AWOTE := 633.60] %>%
  
  .[.("1993-02-19", "Male", "Total"), AWOTE := 512.40] %>%
  .[.("1993-05-21", "Male", "Total"), AWOTE := 519.00] %>%
  
  .[.("1993-02-19", "Female", "Ordinary"), AWOTE := 527.30] %>%
  .[.("1993-05-21", "Female", "Ordinary"), AWOTE := 532.90] %>%
  
  .[.("1993-02-19", "Female", "Total"), AWOTE := 403.40] %>%
  .[.("1993-05-21", "Female", "Total"), AWOTE := 408.10] %>%
  
  .[.("1993-02-19", "Persons", "Ordinary"), AWOTE := 591.90] %>%
  .[.("1993-05-21", "Persons", "Ordinary"), AWOTE := 597.90] %>%
  
  .[.("1993-02-19", "Persons", "Total"), AWOTE := 512.40] %>%
  .[.("1993-05-21", "Persons", "Total"), AWOTE := 519.00] %>%
  
  # http://www.ausstats.abs.gov.au/ausstats/free.nsf/0/761EBD51124BCA23CA2574FA001441D4/$File/63020_AUG1993.pdf
  
  .[.("1993-08-15", "Persons", "Ordinary"), AWOTE := 604.80] %>%
  .[.("1993-08-15", "Male", "Ordinary"), AWOTE := 641.20] %>%
  .[.("1993-08-15", "Female", "Ordinary"), AWOTE := 538.40] %>%
  rbind(AWOTE1994_2011_Data, use.names = TRUE) %>%
  rbind(AWOTE_Data, use.names = TRUE) %>%
  drop_col("Series ID") %>%
  .[, isMale := Sex == "Male"] %>%
  .[, isMale := if_else(Sex == "Persons", NA, isMale)] %>%
  drop_col("Sex") %>%
  .[, isOrdinary := Earnings %ein% "Ordinary"] %>%
  drop_col("Earnings") %>%
  .[, Date := as.Date(Date)] %>%
  set_unique_key(isMale, isOrdinary, isAdult, Date) %>%
  set_cols_first(key(.)) %>%
  .[]

AWOTE_by_fy_isMale_isOrdinary_isAdult <-
  AWOTE_by_Date_isMale_isOrdinary_isAdult %>%
  .[, .(AWOTE = mean(AWOTE)), 
    keyby = .(isMale, 
              isOrdinary,
              isAdult,
              fy_year = date2fy(Date))]

max_AWOTE_fy <- AWOTE_by_fy_isMale_isOrdinary_isAdult[, max(fy_year)]
min_AWOTE_fy <- 
  AWOTE_by_fy_isMale_isOrdinary_isAdult %>%
  
  # complete cases while AWOTE is being filled in. 
  .[complete.cases(.)] %>%
  .[, min(fy_year)]


do_dots <- function(...) {
  eval(substitute(alist(...)))
}

provide.dir("data-raw/sysdata")
fwrite_dots <- function(...) {
  dotsL <- length(match.call(expand.dots = FALSE)$...)
  Vs <- vapply(seq_len(dotsL),
               function(x) deparse(do_dots(...)[[x]]),
               character(1))
  the_dots <- list(...)
  
  for (j in seq_len(dotsL)) {
    cat(Vs[[j]], "\n")
    if (is.data.frame(the_dots[[j]])) {
      if (!identical(class(the_dots[[j]]), c("data.table", "data.frame"))) {
        stop(class(the_dots[[j]]))
      }
      if (is.data.table(the_dots[[j]])) {
        fwrite(the_dots[[j]], file.path("data-raw", "sysdata",  paste0(Vs[[j]], ".tsv")), sep = "\t",
               logical01 = TRUE)
      }
    }
  }
}

use_and_write_data <- function(...) {
  usethis::use_data(..., internal = TRUE, overwrite = TRUE)
  fwrite_dots(...)
}

setkey(cg_inflators_1213, fy_year, forecast.series)
setkey(cg_inflators_1314, fy_year, forecast.series)
setkey(cg_inflators_1415, fy_year, forecast.series)
setkey(cg_inflators_1516, fy_year, forecast.series)

setkey(generic_inflators_1213[, fy_year := NULL], h, variable)
setkey(generic_inflators_1314[, fy_year := NULL], h, variable)
setkey(generic_inflators_1415[, fy_year := NULL], h, variable)
setkey(generic_inflators_1516[, fy_year := NULL], h, variable)

setkey(wages_trend, obsTime)
setindex(wages_trend, obsQtr)
.date_data_updated <- as.Date("2019-02-23") #Sys.Date()

g_pop_forecasts_by_age_range <-
  local({
    #source("./data-raw/population_forecast.R")
    the_populations <- population_forecast(to_year = 2040L,
                                           include_tbl = TRUE, 
                                           do_log = TRUE)
    the_populations[, Age := year(Date) - YOB]
    the_populations <- the_populations[Age %between% c(15L, 90L)][month(Date) == 6L]
    the_populations[, age_range := grattan:::age2age_range(Age)]
    the_populations[, .(Population = sum(Population)), keyby = .(age_range, fy = date2fy(Date))]
  })


library(fastmatch)
fys1901 <- yr2fy(1901:2100)
"1999-00" %fin% fys1901

use_and_write_data(tax_table2, 
                   lito_tbl, 
                   tax_tbl, 
                   medicare_tbl, 
                   sapto_tbl,
                   hecs_tbl,
                   cpi_unadj,
                   cpi_seasonal_adjustment,
                   cpi_trimmed,
                   cpi_unadj_fy,
                   cpi_seasonal_adjustment_fy,
                   cpi_trimmed_fy,
                   wages_trend,
                   wages_trend_fy,
                   lf_trend,
                   lf_trend_fy,
                   cgt_expenditures,
                   mean_of_each_taxstats_var, 
                   meanPositive_of_each_taxstats_var,
                   generic_inflators_1516,
                   generic_inflators_1415,
                   generic_inflators_1314,
                   generic_inflators_1213,
                   cg_inflators_1516,
                   cg_inflators_1415,
                   cg_inflators_1314,
                   cg_inflators_1213,
                   super_contribution_inflator_1314,
                   g_pop_forecasts_by_age_range,
                   #
                   salary_by_fy_swtile,
                   differential_sw_uprates,
                   
                   Age_pension_base_rates_by_year,
                   Age_pension_assets_test_by_year,
                   Age_pension_deeming_rates_by_Date,
                   Age_pension_permissible_income_by_Date,
                   
                   AWOTE_by_Date_isMale_isOrdinary_isAdult,
                   max_AWOTE_fy,
                   min_AWOTE_fy,
                   
                   bto_tbl,
                   aus_pop_by_yearqtr,
                   aust_pop_by_age_yearqtr,

                   abs_key_aggregates,
                   
                   parenting_payment_by_fy,
                   partner_income_free_area_by_fy_student_age,
                   
                   
                   unemployment_income_tests,
                   unemployment_annual_rates,
                   unemployment_assets_tests,
                   unemployment_income_tests_by_date,
                   unemployment_rates_by_date,
                   unemployment_assets_tests_by_date,

                   rent_assistance_rates,
                   rent_assistance_rates_by_date,

                   youth_income_tests,
                   youth_annual_rates,

                   # possibly separable
                   .avbl_fractions,
                   .date_data_updated,
                   fys1901,
                   min.wage.yr,
                   max.wage.yr,
                   #
                   min.cpi_unadj.yr,
                   max.cpi_unadj.yr,
                   min.cpi_seasonal_adjustment.yr,
                   max.cpi_seasonal_adjustment.yr,
                   min.cpi_trimmed.yr,
                   max.cpi_trimmed.yr,
                   #
                   min.lf.yr,
                   max.lf.yr)
