# Income tax tables.
library(magrittr)
library(dplyr)

tax_tbl <-
  data.table::fread("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv")

lito_tbl <- 
  readxl::read_excel("./data-raw/lito-info.xlsx", sheet = 1) %>% dplyr::select(-source)

lito_tbl %>% 
  readr::write_tsv("./data-raw/lito-info.tsv")

medicare_tbl_indiv <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.)

medicare_tbl_families <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "families") %>%
  data.table::as.data.table(.)

medicare_tbl <- 
  data.table::rbindlist(list(medicare_tbl_indiv, medicare_tbl_families), use.names = TRUE, fill = TRUE) %>%
  dplyr::mutate_each(funs(as.logical), sato, pto, sapto) %>%
  data.table::as.data.table(.) %>%
  data.table::setkey(fy_year, sapto, family_status) %>%
  # avoid cartesian joins
  unique

medicare_tbl %>% 
  readr::write_tsv("medicare_tbl.tsv")

sapto_tbl <- 
  readxl::read_excel("./data-raw/SAPTO-rates.xlsx", sheet = 1) %>% 
  data.table::as.data.table(.) %>% 
  data.table::setkey(fy_year, family_status) %>% 
  # avoid cartesian joins in income_tax
  unique

sapto_tbl %>%
  readr::write_tsv("sapto_tbl.tsv")

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
  data.table::fread("./data-raw/cpi-unadjusted-manual.tsv")

cpi_seasonal_adjustment <- 
  data.table::fread("./data-raw/cpi-seasonally-adjusted-manual.tsv", select = c("obsTime", "obsValue")) 

cpi_trimmed <-
  data.table::fread("./data-raw/cpi-trimmed-mean-manual.tsv")

wages_trend <- 
  data.table::fread("./data-raw/wages-trend.tsv")

lf_trend <- 
  data.table::fread("./data-raw/lf-trend.tsv")

cgt_expenditures <- 
  data.table::fread("./data-raw/tax-expenditures-cgt-historical.tsv")


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
                   #
                   internal = TRUE, overwrite = TRUE)
