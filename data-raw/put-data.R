# Income tax tables.

tax_tbl <-
  data.table::fread("./data-raw/tax-brackets-and-marginal-rates-by-fy.tsv")

lito_tbl <- 
  data.table::setkey(data.table::fread("./data-raw/lito-info.tsv"), fy_year)

medicare_tbl_indiv <- 
  readxl::read_excel("./data-raw/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.) %>%
  data.table::setkey(fy_year, sato) %>%
  # avoid cartesian joins
  unique 

sapto_tbl <- 
  readxl::read_excel("./data-raw/SAPTO-rates.xlsx", sheet = 1) %>% 
  data.table::as.data.table(.) %>% 
  data.table::setkey(fy_year, family_status) %>% 
  # avoid cartesian joins in income_tax
  unique

# Manually
cpi_unadj <- 
  data.table::fread("./data-raw/cpi-unadjusted-manual.tsv")

cpi_seasonal_adjustment <- 
  data.table::fread("./data-raw/cpi-seasonally-adjusted-manual.tsv") # bump

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
                   medicare_tbl_indiv, 
                   sapto_tbl,
                   cpi_unadj,
                   cpi_seasonal_adjustment,
                   cpi_trimmed,
                   wages_trend,
                   lf_trend,
                   cgt_expenditures,
                   #
                   internal = TRUE, overwrite = TRUE)
