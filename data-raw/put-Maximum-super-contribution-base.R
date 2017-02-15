library(readxl)
library(dplyr)
library(magrittr)
library(devtools)
library(grattan)

# https://www.ato.gov.au/Rates/key-superannuation-rates-and-thresholds/?page=22#Maximum_super_contribution_base
max_super_contr_base <- 
  read_excel("data-raw/Maximum_super_contribution_base.xlsx", sheet = 1) %>%
  # ensure standard dashes and 1999-2000 --> 1999-00
  mutate(fy_year = yr2fy(as.numeric(gsub("^([12][0-9]{3}).*$", "\\1", `Financial year`)) + 1)) %>%
  select(fy_year, max_sg_per_qtr = `Per quarter`)

use_data(max_super_contr_base)
