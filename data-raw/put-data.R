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

.cpi_unadj <- data.table::as.data.table(cpi)  
.cpi_seasonal_adjustment <- data.table::as.data.table(cpi) 
.cpi_trimmed <- data.table::as.data.table(cpi) 


devtools::use_data(.lito_tbl, 
                   .tax_tbl, 
                   .medicare.tbl.indiv, 
                   .sapto_tbl,
                   .cpi_unadj,
                   .cpi_seasonal_adjustment,
                   .cpi_trimmed,
                   internal = TRUE, overwrite = TRUE)
