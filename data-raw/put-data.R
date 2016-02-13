.lito_tbl <- 
  setkey(fread("data/lito-info.tsv"), fy_year)

.medicare.tbl.indiv <- 
  readxl::read_excel("./data/medicare-tables.xlsx", sheet = "indiv") %>%
  data.table %>%
  setkey(fy_year)

.sapto_tbl <- 
  readxl::read_excel("./data/SAPTO-rates.xlsx", sheet = 1) %>%
  data.table %>%
  setkey(fy_year, family_status, rebate_income)

devtools::use_data(.lito_tbl, tax_tbl, 
                   .medicare.tbl.indiv, 
                   .sapto_tbl,
                   internal = TRUE, overwrite = TRUE)
