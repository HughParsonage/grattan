#' Inverse income tax functions
#' 
#' @param tax the tax payable 
#' @return the taxable income (integer) given the tax payable for the financial year, acurrate to the nearest integer

inverse_income <- function(tax, fy.year = "2012-13"){
  income <- 0L
  while(grattan::income_tax(income, fy.year = fy.year) <= tax){
    income <- income + 1000L
  }
  
  income <- income - 999L
  
  while(grattan::income_tax(income, fy.year = fy.year) <= tax){
    income <- income + 1L
  }
  
  return(income)
}

inverse_income_lookup <- function(tax, fy.year = "2012-13", zero.tax = "maximum"){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  
  input <- data.table::data.table(taxes = tax)
  temp <- data.table::setkey(
    data.table::data.table(incomes = income.range, 
                           taxes = grattan::income_tax(income.range, 
                                                       fy.year = fy.year)),
    taxes)
  
  tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  return(tbl$incomes)
}

inverse_income_lookup2 <- function(tax, fy.year = "2012-13", zero.tax = "maximum"){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  input <- data.table::data.table(taxes = tax)
  temp <- data.table::data.table(incomes = income.range)
  temp$taxes <- grattan::income_tax(temp$incomes, fy.year = fy.year)
  data.table::setkey(temp, taxes)
  tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  return(tbl$incomes)
}

