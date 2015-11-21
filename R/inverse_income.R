#' Inverse income tax functions
#' 
#' @param tax the tax payable 
#' @param zero.tax.income ("maximum", "zero", "uniform", numeric()) Given that many incomes map to zero taxes, the \code{income_tax} function is not invertible there. As a consequence, the inverse function's value must be specified for tax = 0. "maximum" returns the maximum integer income one can have with a zero tax liability; "zero" returns zero for any tax of zero; "uniform" provides a random integer from zero to the maximum income with a zero tax. The value can also be specified explicitly.
#' @return the taxable income (integer) given the tax payable for the financial year, acurrate to the nearest integer
#' 

inverse_income <- 
  function(tax, fy.year = "2012-13", zero.tax.income = c("maximum", "zero", "uniform", numeric(1)), ...){ 
    zero.tax.income <- zero.tax.income[1]
    if (length(tax) > 1)
      inverse_income_lookup3(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...)
    else
      inverse_income_lengthone(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...)
  }

inverse_income_while <- function(tax, fy.year = "2012-13", ...){
  if(is.na(tax))
    return(tax)
  else {
    income <- 0L
    while(grattan::income_tax(income, fy.year = fy.year, ...) <= tax){
      income <- income + 1000L
    }
    
    income <- income - 999L
    
    while(grattan::income_tax(income, fy.year = fy.year, ...) <= tax){
      income <- income + 1L
    }
    
    return(income)
  }
}

inverse_income_lengthone <- function(tax, fy.year = "2012-13", zero.tax.income = c("maximum", "zero", "uniform", numeric(1)), ...){
  if (tax == 0){
    if (zero.tax.income == "zero"){
      out <- 0L
    } else {
      if (is.numeric(zero.tax.income)){
        out <- zero.tax.income
      } else {
        maximum.zero.tax.income <- inverse_income_while(0, fy.year = fy.year, ...)
        if(zero.tax.income == "maximum"){
          out <- maximum.zero.tax.income
        } else {
          if (zero.tax.income == "uniform"){
            out <- sample(0:maximum.zero.tax.income, size = 1L)
          }
        }
        
      }
    }
  } else {
    out <- inverse_income_while(tax, fy.year = fy.year, ...)
  }
  
  return(out)
}
  
inverse_income_lookup <- function(tax, fy.year = "2012-13", zero.tax = "maximum", ...){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  
  input <- data.table::data.table(taxes = tax)
  temp <- data.table::setkey(
    data.table::data.table(incomes = income.range, 
                           taxes = grattan::income_tax(income.range, 
                                                       fy.year = fy.year, ...)),
    taxes)
  
  tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  return(tbl$incomes)
}

inverse_income_lookup2 <- function(tax, fy.year = "2012-13", zero.tax = "maximum", ...){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  input <- data.table::data.table(taxes = tax)
  temp <- data.table::data.table(incomes = income.range)
  temp$taxes <- grattan::income_tax(temp$incomes, fy.year = fy.year, ...)
  data.table::setkey(temp, taxes)
  # LOOCF, all taxes are invertible
  # tbl <- temp[input, roll=-Inf]  dispatches data.frame
  tbl <- data.table:::`[.data.table`(temp, input, roll=-Inf)
  return(tbl$incomes)
}

inverse_income_lookup3 <- function(tax, fy.year = "2012-13", zero.tax.income = "maximum", ...){
  NAs <- is.na(tax)
  tax <- ifelse(NAs, 1L, tax)
  # This function creates a lookup table of incomes taxes 
  zeroes <- !NAs & tax == 0
  # We now designate the range of incomes to search over. 
  # Always check up to $100,000. There, the ratio of income to 
  # tax is at most 3.79 and decreases thereafter (2012-13).
  income.range <- seq(0L, max(ceiling(max(tax) * 3.79), 100000L), by = 1L)  
  input <- data.table::data.table(taxes = ifelse(zeroes, 1, tax))  # ensure a one-to-one relationship
  temp <- data.table::data.table(incomes = income.range)
  temp$taxes <- grattan::income_tax(temp$incomes, fy.year = fy.year, ...)
  data.table::setkey(temp, taxes)
  # tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  tbl <- data.table:::`[.data.table`(temp, input, roll=-Inf)
  
  out <- tbl$incomes
  # Take care of zeroes
  if(any(zeroes)){
    if (zero.tax.income == "zero"){
      out[zeroes] <- 0L
    } else {
      if(is.numeric(zero.tax.income)){
        out[zeroes] <- zero.tax.income
      } else {
        max.zero.tax.income <- inverse_income_while(0, fy.year = fy.year, ...) - 1
        if (zero.tax.income == "maximum"){
          out[zeroes] <- max.zero.tax.income
        } else {
          if(zero.tax.income == "uniform")
            out[zeroes] <- sample(0:max.zero.tax.income, size = sum(zeroes))
        }
      }
    }
  }
  # keep the NAs
  out[NAs] <- NA
  return(out)
}

