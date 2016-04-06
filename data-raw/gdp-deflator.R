NULL
# library(readxl)
# library(dplyr)
# library(data.table)
# library(tidyr)
# # 
# metadata <- 
#   read_excel("./inst/extdata/ABS/5206001_key_aggregates.xlsx", sheet = "Data1", col_names = FALSE) %>%
#   # refer just to the header
#   filter(row_number(1:n()) <= 10) %>%
#   gather(Series, metadata, -X0) %>%
#   # The top left cell is blank, but this refers to the name of the cell
#   mutate(X0 = ifelse(is.na(X0), "Series_name", X0)) %>%
#   group_by(Series) %>%
#   mutate(Series_id = metadata[which(X0 == "Series ID")]) %>%
#   spread(X0, metadata) %>%
#   select(Series_id, Series_name, `Series Type`) %>%
#   mutate(Series_short_name = gsub("((\\s)|(:))+", "_", paste(
#                                         gsub("Gross domestic product", 
#                                              "GDP",
#                                              gsub("Chain volume measures",
#                                                   "Chain_Real", 
#                                                   gsub("Current prices", 
#                                                        "Nominal",
#                                                        gsub("\\s*;", 
#                                                             "",
#                                                             Series_name)))), `Series Type`, sep = "_")))   
# 
# .abs_cat_5206_001 <-
# read_excel("./inst/extdata/ABS/5206001_key_aggregates.xlsx", sheet = "Data1", skip = 9) %>%
#   data.table %>%
#   setnames(1, "obsDate") %>%
#   setnames(old = metadata$Series_id, new = metadata$Series_short_name) 
# 
# .gdp_deflator_data <- 
#   .abs_cat_5206_001 %>%
#   select(obsDate, GDP_Chain_Real_Trend, GDP_Nominal_Trend) %>%
#   mutate(gdp_deflator = GDP_Nominal_Trend / GDP_Chain_Real_Trend,
#          gdp_deflator = gdp_deflator / last(gdp_deflator)) %>% 
# 
# gdp_deflator <- function(from, to){
#   
#   
# }
#   