#' Senior Australians and Pensioner Tax Offset

sapto <- function(rebate_income, fy_year, family_status = "single"){
  input <- data.table::data.table(fy_year = fy_year, 
                                  family_status = family_status, 
                                  rebate_income = rebate_income, 
                                  key = c("fy_year", "family_status", "rebate_income"))
  setkey(.sapto_tbl, fy_year, family_status, rebate_income)
  
  .sapto_tbl[input, roll = Inf]
}