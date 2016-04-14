library(readxl)
library(dplyr)
library(tidyr)
library(httr)

if (!file.exists("./data-raw/Individuals_table1_2013-14.xlsx")){
  GET(url = "http://data.gov.au/dataset/25e81c18-2083-4abe-81b6-0f530053c63f/resource/3cd6dee1-785f-4876-a282-ebcf00f9949a/download/Taxstats2014Individual01SelectedItemsByYear.xlsx", 
      write_disk("./data-raw/Individuals_table1_2013-14.xlsx"))
}

read_excel("data-raw/Individuals_table1_2013-14.xlsx", sheet = 2, skip = 2)