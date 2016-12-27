#' Financial functions
#' @description Financial functions from Excel. These functions are equivalent to the Excel functions of the same name (in uppercase).
#' @name npv
#' @aliases irr fv pv pmt
#' \url{http://cvs.moodle.org/contrib/patches/question_calculated_extended/calculated/packages/financial/financial_class.php?view=co}
#' @param x Cash flow.
#' @param start Initial guess to start the iterative process.
#' @param fv Future value.
#' @param rate Discount or interest rate.
#' @param values Income stream.
#' @param nper Number of periods
#' @param pmt Payments.
#' @param pv Present value.
#' @param type Factor.
#' @examples
#' npv(0.07, c(1, 2))
#' @author Enrique Garcia M. \email{egarcia@@egm.as}
#' @author Karsten W. \email{k.weinert@@gmx.net}
#' @export npv irr fv pv pmt
npv <- function(rate, values){
  sum(values / (1 + rate) ^ seq_along(values))
}
#' @rdname npv
#' @examples
#' irr(x = c(1, -1), start = 0.1)
irr <- function(x, start = 0.1){
  if (uniqueN(sign(x)) != 2){
    stop("At least one value in 'x' must be negative and one must be positive.")
  }
  t <- seq_along(x) - 1
  f <- function(i){
    abs(sum(x / (1+i) ^ t))
  }
  return(stats::nlm(f, start)$estimate)
}
#' @rdname npv
#' @examples
#' fv(0.04, 7, 1, pv = 0.0, type = 0)
fv <- function(rate, nper, pmt, pv = 0.0, type = 0) {
  pvif <- (1 + rate) ^ nper # Present value interest factor
  fvifa <- 
    if_else(near(rate, 0), 
            nper, 
            ((1 + rate) ^ nper - 1) / rate)
  return(-1 * ((pv * pvif) + pmt * (1.0 + rate * type) * fvifa))
}
#' @rdname npv
#' @examples
#' pv(rate = 0.08, nper = 7, pmt = 1, fv = 0.0, type = 0)
pv <- function(rate, nper, pmt, fv = 0.0, type = 0) {
  pvif <- (1 + rate) ^ nper # Present value interest factor
  fvifa <- 
    if_else(near(rate, 0), 
            nper, 
            ((1 + rate) ^ nper - 1) / rate)
  return((-fv - pmt * (1.0 + rate * type) * fvifa) / pvif)
}
#' @rdname npv
#' @examples
#' pmt(rate = 0.025, nper = 7, pv = 0, fv = 0.0, type = 0)
pmt <- function(rate, nper, pv, fv = 0, type = 0) {
  rr <- 1 / (1 + rate) ^ nper
  res <- (-pv - fv * rr) * rate / (1 - rr)
  return(res / (1 + rate * type))
}
