#' Inverse average tax rate
#' @param average_rate The average tax rate (\eqn{\frac{tax}{income}})
#' @param ... Parameters passed to \code{\link{income_tax}}.
#' @param .max The maximum income to test before ending the search. (Used only to prevent infinite loops.)
#' @return The minimum income at which the average tax rate exceeds \code{average_rate}.
#' @examples 
#' inverse_average_rate(0.2, fy.year = "2014-15")
#' @export

inverse_average_rate <- function(average_rate, ..., .max = 100e6){
  stopifnot(length(average_rate) == 1L)
  stopifnot(average_rate > 0)  
  
  x <- 2^14
  
  # for incomes below 2^14
  while (income_tax(x, ...) / x > average_rate){
    x <- x - 2
  }
  
  while (income_tax(x, ...) / x <= average_rate){
    x <- x + 2^14
    if (x > .max){
      stop("Stopping search due to possibly infinite income for given average rate. Check inputs or increase .max.")
    }
  }
  x <- x - 2^14
  
  while (income_tax(x, ...) / x <= average_rate){
    x <- x + 2^12
  }
  x <- x - 2^12
  
  while (income_tax(x, ...) / x <= average_rate){
    x <- x + 2^9
  }
  x <- x - 2^9
  
  while (income_tax(x, ...) / x <= average_rate){
    x <- x + 2^6
  }
  x <- x - 2^6
  
  while (income_tax(x, ...) / x <= average_rate){
    x <- x + 1
  }
  x
}
