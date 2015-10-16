#' Inverse income tax functions
#' 
#' @param tax the tax payable 
#' @param zero.tax.income ("maximum", "zero", numeric()) Given that many incomes map to zero taxes, the \code{income_tax} function is not invertible there. As a consequence, the inverse function's value must be specified for tax = 0. "maximum" returns the maximum integer income one can have with a zero tax liability; "zero" returns zero for any tax of zero. The value can also be specified explicitly.
#' @return the taxable income (integer) given the tax payable for the financial year, acurrate to the nearest integer
#' 

inverse_income <- function(...) inverse_income_lookup3(...)

inverse_income_while <- function(tax, fy.year = "2012-13"){
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
  # LOOCF, all taxes are invertible
  # tbl <- temp[input, roll=-Inf]  dispatches data.frame
  tbl <- data.table:::`[.data.table`(temp, input, roll=-Inf)
  return(tbl$incomes)
}

inverse_income_lookup3 <- function(tax, fy.year = "2012-13", zero.tax.income = "maximum"){
  zeroes <- tax == 0
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  input <- data.table::data.table(taxes = ifelse(zeroes, 1, tax))
  temp <- data.table::data.table(incomes = income.range)
  temp$taxes <- grattan::income_tax(temp$incomes, fy.year = fy.year)
  data.table::setkey(temp, taxes)
  # tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  tbl <- data.table:::`[.data.table`(temp, input, roll=-Inf)
  
  # Take care of zeroes
  out <- tbl$incomes
  if(length(out) == 0)
    out <- numeric(1)
  
  if(any(zeroes)){
    if (zero.tax.income == "maximum"){
      out[zeroes] <- inverse_income_while(0, fy.year = fy.year) - 1
    } else {
      if (zero.tax.income == "zero"){
        out[zeroes] <- 0L
      } else {
        out[zeroes] <- zero.tax.income
      }
    }
  }
  return(out)
}

