.tax_tbl <-
  data.table::fread("./data/tax-brackets-and-marginal-rates-by-fy.tsv")

.lito_tbl <- 
  data.table::setkey(data.table::fread("data/lito-info.tsv"), fy_year)

.medicare.tbl.indiv <- 
  readxl::read_excel("./data/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table::as.data.table(.) %>%
  data.table::setkey(fy_year)

.sapto_tbl <- 
  readxl::read_excel("./data/SAPTO-rates.xlsx", sheet = 1)

# Manually
.cpi_unadj <- 
  data.table::fread("./data/cpi-unadjusted-manual.tsv")

.cpi_seasonal_adjustment <- 
  data.table::fread("./data/cpi-seasonally-adjusted-manual.tsv") # bump

.cpi_trimmed <-
  data.table::fread("./data/cpi-trimmed-mean-manual.tsv")

.wages_trend <- 
  data.table::fread("./data/wages-trend.tsv")

.lf_trend <- 
  data.table::fread("./data/lf-trend.tsv")


devtools::use_data(.lito_tbl, 
                   .tax_tbl, 
                   .medicare.tbl.indiv, 
                   .sapto_tbl,
                   .cpi_unadj,
                   .cpi_seasonal_adjustment,
                   .cpi_trimmed,
                   .wages_trend,
                   .lf_trend,
                   internal = TRUE, overwrite = TRUE)
