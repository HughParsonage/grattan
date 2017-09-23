#' Inverse income tax functions
#' 
#' @param tax The tax payable.
#' @param fy.year The relevant financial year.
#' @param zero.tax.income A character vector, ("maximum", "zero", "uniform", numeric(1)) Given that many incomes map to zero taxes, the \code{income_tax} function is not invertible there. As a consequence, the inverse function's value must be specified for tax = 0. "maximum" returns the maximum integer income one can have with a zero tax liability; "zero" returns zero for any tax of zero; "uniform" provides a random integer from zero to the maximum income with a zero tax. The value can also be specified explicitly.
#' @param ... Other arguments passed to \code{income_tax}. If \code{tax} or \code{fy.year} are vectors, these should be named vectors.
#' @return The approximate taxable income given the tax payable for the financial year. See Details.
#' @details This function has an error of $2.
#' @export inverse_income
#' 

inverse_income <- function(tax, fy.year = "2012-13", zero.tax.income = c("maximum", "zero", "uniform", numeric(1)), ...){
  prohibit_vector_recycling(tax, fy.year)
  max_lengths <- max(length(tax), length(fy.year))
  if (!is.numeric(zero.tax.income)){
    zero.tax.income <- match.arg(zero.tax.income)
  }
  if (any(tax[!is.na(tax)] < 0))
    stop("tax must be nonnegative")
  
  if (max_lengths > 1){
    out <- inverse_income_lookup3(tax, fy.year = fy.year, zero.tax.income = zero.tax.income, ...)
  } else {
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

inverse_income_lookup3 <- function(tax, fy.year = "2012-13", zero.tax.income = "maximum", ...){
  NAs <- is.na(tax)
  tax <- if_else(NAs, if (is.integer(tax)) 1L else 1.0, tax)
  infs <- is.infinite(tax)
  tax <- if_else(infs, if (is.integer(tax)) 1L else 1.0, tax)
  oo <- rank(tax, ties.method = "first")
  # This function creates a lookup table of incomes taxes 
  zeroes <- !NAs & tax == 0
  # We now designate the range of incomes to search over. 
  # Always check up to $100,000. There, the ratio of income to 
  # tax is at most 3.79 and decreases thereafter (2012-13).
  income.range <- seq(0L, max(ceiling(max(tax) * 3.79), 100000L), by = 1L) 
  fy..year <- taxes <- NULL
  if (identical(list(...), list())){
    input <- data.table(taxes = if_else(zeroes,
                                        if (is.integer(tax)) 1L else 1.0, 
                                        tax),   # ensure a one-to-one relationship
                        fy..year = fy.year)
  } else {
    input <- data.table(taxes = if_else(zeroes,
                                        if (is.integer(tax)) 1L else 1.0,
                                        tax),   # ensure a one-to-one relationship
                        fy..year = fy.year, 
                        ...)
  }
  incomes <- NULL
  temp <- data.table(incomes = income.range, 
                     fy..year = fy.year)
  # temp[, taxes := income_tax(incomes, fy.year = fy.year, ...)]
  
  temp[, taxes := income_tax(incomes, fy.year = fy..year, ...)]
  setkeyv(temp, c("fy..year", "taxes"))
  setkeyv(input, c("fy..year", "taxes"))
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
  out[infs] <- Inf
  return(out)
}

inverse_income_which_min <- function(tax, fy.year, zero.tax.income = "maximum", ...){
  if (is.infinite(tax)) return(Inf)
  min(which(income_tax(1:(tax * 3 + 40e3), fy.year = fy.year, ...) > tax))
}

