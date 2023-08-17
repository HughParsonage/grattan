#' CPI for general dates
#' 
#' @description Deprecated in favour of \code{grattanInflators::cpi_inflator}
#' 
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_date (character, date-like) the 'date' contemporaneous to \code{from_nominal_price}. The acceptable forms are 'YYYY', 'YYYY-YY' (financial year), 'YYYY-MM-DD', and 'YYYY-Q[1-4]' (quarters). Note a vector cannot contain a mixture of date forms.
#' @param to_date (character, date-like) the date at which the real price is valued (where the nominal price equals the real price). Same forms as for \code{from_date}
#' @param ... other arguments passed to \code{\link{cpi_inflator}}
#' 
#' @return A vector of real prices in \code{to_date} dollars.
#' @export

cpi_inflator_general_date <- function(from_nominal_price = 1, 
                                      from_date, 
                                      to_date, 
                                      ...) {
  warning("cpi_inflator_general_date is deprecated. Use grattanInflators::cpi_inflator")
  grattanInflators::cpi_inflator(x = from_nominal_price, from = from_date, to = to_date)
}

