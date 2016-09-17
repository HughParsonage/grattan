#' Inverse income tax functions
#' 
#' @param tax The tax payable.
#' @param fy.year The relevant financial year.
#' @param zero.tax.income A character vector, ("maximum", "zero", "uniform", numeric(1)) Given that many incomes map to zero taxes, the \code{income_tax} function is not invertible there. As a consequence, the inverse function's value must be specified for tax = 0. "maximum" returns the maximum integer income one can have with a zero tax liability; "zero" returns zero for any tax of zero; "uniform" provides a random integer from zero to the maximum income with a zero tax. The value can also be specified explicitly.
#' @param ... Other arguments passed to \code{income_tax}.
#' @return The approximate taxable income given the tax payable for the financial year. See Details.
#' @details The number returned has an error of $2.
#' @export inverse_income
#' 

inverse_income <- function(tax, fy.year = "2012-13", zero.tax.income = c("maximum", "zero", "uniform", numeric(1)), ...){
  if (!is.numeric(zero.tax.income)){
    zero.tax.income <- match.arg(zero.tax.income)
  }
  if(any(tax < 0))
    stop("tax must be nonnegative")
  
  if (length(tax) > 1)
    out <- inverse_income_lookup3(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...)
  else {
    if (tax < .Machine$double.eps ^ 0.5){
      if (zero.tax.income == "zero"){
        out <- 0
      } else {
        if (is.numeric(zero.tax.income)){
          out <- zero.tax.income
        } else {
          if (zero.tax.income == "maximum"){
            out <- inverse_income_which_min(tax, fy.year = fy.year, zero.tax.income = "maximum", ...)
          } else {
            out <- (sample(1:inverse_income_which_min(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...), 
                           size = 1))
          }
        }
      }
    } else {
      out <- inverse_income_which_min(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...)
    }
  }
  out
}

inverse_income_radix <- function(tax, fy.year = "2012-13", ...){
  stopifnot(length(tax) == 1L)
  if (is.na(tax))
    return(tax)
  else {
    income <- 0L
    step <- 2^15
    while (income_tax(income, fy.year = fy.year, ...) <= tax){
      income <- income + step
    }
    step <- step / 2
    
    income <- income - 2^14
    diff <- income_tax(income, fy.year = fy.year, ...) - tax
    
    while (step > 0.125 || abs(diff) > 1){
      if (diff < 0){
        income <- income + step
      } else {
        income <- income - step
      }
      diff <- income_tax(income, fy.year = fy.year, ...) - tax
      step <- step / 2
    }
    income
  }
}

inverse_income_while <- function(tax, fy.year = "2012-13", ...){
  if (is.na(tax))
    return(tax)
  else {
    income <- 0L
    while (income_tax(income, fy.year = fy.year, ...) <= tax){
      income <- income + 1000L
    }
    
    income <- income - 999L
    
    while(income_tax(income, fy.year = fy.year, ...) <= tax){
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
        maximum.zero.tax.income <- inverse_income_which_min(0, fy.year = fy.year, ...)
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
    out <- inverse_income_radix(tax, fy.year = fy.year, ...)
  }
  
  return(out)
}
  
inverse_income_lookup <- function(tax, fy.year = "2012-13", zero.tax = "maximum", ...){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  
  input <- data.table(taxes = tax)
  temp <- 
    setkeyv(data.table(incomes = income.range, 
                       taxes = income_tax(income.range, fy.year = fy.year, ...)),
            "taxes")
  
  tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  tbl[["incomes"]]
}

inverse_income_lookup2 <- function(tax, fy.year = "2012-13", zero.tax = "maximum", ...){
  income.range <- seq(0L, max(ceiling(max(tax) * 3), 100000L), by = 1L)
  input <- data.table(taxes = tax)
  temp <- data.table(incomes = income.range)
  temp$taxes <- income_tax(temp$incomes, fy.year = fy.year, ...)
  data.table::setkeyv(temp, "taxes")
  # LOOCF, all taxes are invertible
  tbl <- temp[input, roll = -Inf]
  tbl$incomes
}

inverse_income_lookup3 <- function(tax, fy.year = "2012-13", zero.tax.income = "maximum", ...){
  NAs <- is.na(tax)
  tax <- dplyr::if_else(NAs, 1, tax)
  oo <- rank(tax, ties.method = "first")
  # This function creates a lookup table of incomes taxes 
  zeroes <- !NAs & tax == 0
  # We now designate the range of incomes to search over. 
  # Always check up to $100,000. There, the ratio of income to 
  # tax is at most 3.79 and decreases thereafter (2012-13).
  income.range <- seq(0L, max(ceiling(max(tax) * 3.79), 100000L), by = 1L)  
  input <- data.table(taxes = dplyr::if_else(zeroes, 1, tax))  # ensure a one-to-one relationship
  temp <- data.table(incomes = income.range)
  # temp[, taxes := income_tax(incomes, fy.year = fy.year, ...)]
  
  temp[, "taxes" := lapply(.SD, income_tax, fy.year = fy.year), .SDcols = "incomes"]
  setkeyv(temp, "taxes")
  setkeyv(input, "taxes")
  tbl <- temp[input, roll=-Inf]  # LOOCF, all taxes are invertible
  
  out <- tbl[["incomes"]][oo]
  # Take care of zeroes
  if(any(zeroes)){
    if (zero.tax.income == "zero"){
      out[zeroes] <- 0
    } else {
      if(is.numeric(zero.tax.income)){
        out[zeroes] <- zero.tax.income
      } else {
        max.zero.tax.income <- inverse_income_which_min(0, fy.year = fy.year, ...) - 1
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

inverse_income_which_min <- function(tax, fy.year, zero.tax.income = "maximum", ...){
  min(which(income_tax(1:(tax * 3 + 40e3), fy.year = fy.year, ...) > tax))
}

