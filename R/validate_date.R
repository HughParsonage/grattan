#' Verifying validity of dates
#' 
#' @description Many functions expect Dates. 
#' Determining that they are validly entered is often quite 
#' computationally costly, relative to the core calculations.
#' These internal functions provide mechanisms to check validity
#' quickly, while still providing clear, accurate error messages.
#' 
#' @param date_to_verify (character) A user-provided value, purporting to be
#' character vector of dates.
#' @param from,to Indicating the range of years valid for \code{date_to_verify}. Default set to -Inf and Inf respectively (i.e. there is no bound)
#' @param deparsed The name of variable to appear in error messages.
#' 
#' @return \code{date_to_verify} as a Date object, provided it can be converted 
#' to a Date and all elements are within the bounds \code{from} and \code{to}.
#' 

validate_date <- function(date_to_verify, from = NULL, to = NULL,
                        deparsed = "Date") {

  if (!is.Date(date_to_verify)) {
    if (is.character(date_to_verify)) {
      date_to_verify <- 
        tryCatch(as.Date(date_to_verify), 
                 error = function(e) {
                   m <- e$m
                   stop("`", deparsed, "` was not of class 'Date' and ", 
                        "during coercion to Date the following error ", 
                        "was encountered. Ensure `", deparsed, "` is a Date. ", 
                        e$m,
                        call. = FALSE)
                 })
      if (anyNA(date_to_verify)) {
        i_first_bad <- which.max(is.na(date_to_verify)) 
        stop("Position ", i_first_bad, " was not converted to Date successfully.")
      }
      
    } else {
      stop("`Date` was a ", class(date_to_verify)[1], ", but ", 
           "must be a character or Date object.")
    }
  }
  
  # If from and to are fine, we're done
  if (is.null(from) && is.null(to)) {
    return(date_to_verify)
  }
  
  if (!is.null(from) && length(from) != 1L) {
    stop("`from` must be length-one.")
  }
  if (!is.null(to) && length(to) != 1L) {
    stop("`to` must be length-one.")
  }
  
  range2Date <- function(x, from = TRUE) {
    if (is.null(x)) {
      return(NULL)
    }
    # accepts years, financial years, character dates
    if (is.Date(x)) {
      return(x)
    }
    
    if (is.numeric(x)) {
      #  A year, if from then 1st Jan, either 31 Dec
      # Must be integrish or sprintf will (helpfully)
      # throw error
      if (from) {
        return(as.Date(sprintf("%d-01-01", x)))
      } else {
        return(as.Date(sprintf("%d-12-31", x)))
      }
    }
    
    # nocov start
    if (!is.character(x)) {
      stop("Internal error: must be character at this point.")
    }
    # nocov end
    
    if (is.fy(x)) {
      return(fy2date(x))
    }
    
    as.Date(x)
  }
  
  from <- range2Date(from)
  to   <- range2Date(to, FALSE)
  
  if (!is.null(to) && !is.null(from)) {
    if (to < from) {
      stop("`to = ", deparse(substitute(to)),
           "` precedes `from = ", 
           deparse(substitute(from)), "`.")
    }
  }
  
  
  if (!is.null(from) && min(date_to_verify) < from) {
    i_bad_date <- which.min(date_to_verify)
    first_bad <- date_to_verify[i_bad_date]
    stop("`Date` contained '", first_bad, "' at position ",
         i_bad_date, ", earlier than the earliest permitted ", 
         "date: '", as.character(from), "'.")
  }
  
  if (!is.null(to) && max(date_to_verify) > to) {
    i_bad_date <- which.max(date_to_verify)
    first_bad <- date_to_verify[i_bad_date]
    stop("`Date` contained '", first_bad, "' at position ",
         i_bad_date, ", later than the latest permitted ", 
         "date: '", as.character(to), "'.")
  }
 
  return(date_to_verify)
}
