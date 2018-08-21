#' Verifying validity of dates
#' 
#' @description Many functions expect Dates. 
#' Determining that they are validly entered is often quite 
#' computatationally costly, relative to the core calculations.
#' These internal functions provide mechanisms to check validity
#' quickly, while still providing clear, accurate error messages.
#' 
#' @param date_to_verify (character) A user-provided value, purporting to be
#' character vector of dates.
#' @param from,to (integer) Indicating the range of years valid for date_to_verify. Default set to -Inf and Inf respectively (i.e. there is no bound)
#' 
#' @return If \code{date_to_verify} contains valid dates
#' they are returned as date objects. If they were
#' already in that form, they obtain the following attributes:
#' \describe{
#' \item{\code{grattan_all_date}}{\code{TRUE} if all the dates are valid.}
#' 

verify_date <- function(date_to_verify, from = -Inf, to = Inf) {

  if (isTRUE(attr(date_to_verify, "grattan_all_date"))) {
    # If from and to are fine, we're done
    if (missing(from) && missing(to)) {
      return(date_to_verify)
    }
  }
  
  # check if able to make object a Date
  if (any(is.na(as.Date(date_to_verify, optional = TRUE)))) {
    i_bad_date <- which(is.na(as.Date(date_to_verify, optional = TRUE)))
    first_bad_date_i <- i_bad_date[1]
    first_bad_date <- date_to_verify[first_bad_date_i]
    stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ")
  }
  
  # check if in correct timeframe
  if (!all(between(year(date_to_verify), from, to))) {
    i_bad_date <- which(!between(year(date_to_verify), from, to))
    first_bad_date_i <- i_bad_date[1]
    first_bad_date <- date_to_verify[first_bad_date_i]
    stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ",
         "Ensure `Date` only includes dates between ", from, " and ", to)
  }
  #give output tick of approval
  attr(date_to_verify, "grattan_all_date") <- TRUE
  
  return(date_to_verify)
}
