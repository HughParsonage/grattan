#' CPI inflator when dates are nice
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_qtr (date in quarters) the dates contemporaneous to the prices in from_nominal_price. Must be of the form "YYYY-Qq" e.g. "1066-Q2". Q1 = Mar, Q2 = Jun, Q3 = Sep, Q4 = Dec.
#' @param to_qtr (date in quarters) the date to be inflated to, where nominal price = real price. Must be of the form "YYYY-Qq" e.g. "1066-Q2".
#' @param adjustment Should there be an adjustment made to the index? Adjustments include 'none' (no adjustment), 'seasonal', or 'trimmed' [referring to trimmed mean]. By default, \code{seasonal}.
#' @param useABSConnection Ignored.
#' The internal data was updated on 2022-01-03 to 2021-Q3.
#' Using \code{useABSConnection = TRUE} is no longer supported for server issues. 
#' @return A vector of real prices.
#' @export

cpi_inflator_quarters <- function(from_nominal_price = 1,
                                  from_qtr,
                                  to_qtr,
                                  adjustment = c("seasonal", "trimmed", "none"),
                                  useABSConnection = FALSE) {
  stopifnot(isFALSE(useABSConnection))
  series <- 
    switch(match.arg(adjustment), 
           "trimmed" = "trimmed.mean",
           "none" = "original",
           match.arg(adjustment))
  
  message("cpi_inflator_quarters is deprecated. Use grattanInflator::cpi_inflator instead")
  if (!missing(from_nominal_price)) {
    warning("from_nominal_price is deprecated")
  }
  from_nominal_price * grattanInflators::cpi_inflator(from_qtr, to_qtr, series = series)
}
