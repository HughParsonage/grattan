#' Inverse income tax function
#' 
#' @param tax the tax payable 
#' @return the taxable income (integer) given the tax payable for the financial year, acurrate to the nearest integer

inverse_income <- function(tax, fy.year = "2012-13"){
  income <- 0L
  while(income_tax(income, fy.year = fy.year) <= tax){
    income <- income + 1000L
  }
  
  income <- income - 999L
  
  while(income_tax(income, fy.year = fy.year) <= tax){
    income <- income + 1L
  }
  
  return(income)
}