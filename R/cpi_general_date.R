#' CPI for general dates
#' 
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_date (character, date-like) the 'date' contemporaneous to \code{from_nominal_price}. The acceptable forms are 'YYYY', 'YYYY-YY' (financial year), 'YYYY-MM-DD', and 'YYYY-Q[1-4]' (quarters). Note a vector cannot contain a mixture of date forms.
#' @param to_date (character, date-like) the date at which the real price is valued (where the nominal price equals the real price). Same forms as for \code{from_date}
#' @param ... other arguments passed to \code{\link{cpi_inflator_quarters}}
#' 
#' @return A vector of real prices in \code{to_date} dollars.
#' @export

cpi_inflator_general_date <- function(from_nominal_price = 1, 
                                      from_date, 
                                      to_date, 
                                      ...) {
  # Check the nominal date
  if (all(grepl("^[12][0-9]{3}$", from_date))){
    from_date <- paste0(from_date, "-Q4")
    message("CPI: Using Q4 for each year")
  } else {
    
    if (all_fy(from_date)) {
      from_date <- paste0(fy2yr(from_date), "-Q1")
    } else {
      
      if (all(!is.na(strptime(from_date, format = "%Y-%m-%d")))){
        from_date.qtr <- zoo::as.yearqtr(as.Date(from_date))
        from_date <- gsub("\\s", "-", from_date.qtr)
      } else {
        if (all(grepl("^[0-9]{4}.Q[1-4]$", from_date))){
          # Nothing to be done.
        } else {
          stop("Emergency stop: from_date could not be identified.")
        }
      }
    }
  }
  # Target date
  if (all(grepl("^[12][0-9]{3}$", to_date))){
    to_date <- paste0(to_date, "-Q4")
    message("CPI: Using Q4 for each year")
  } else {
    
    if (all(grepl("^[12][0-9]{3}.[0-9]{2}$", to_date))){

      year.ending <- as.numeric(gsub("([12][0-9]{3}).*$", "\\1", to_date)) + 1
      to_date <- paste0(year.ending, "-Q1")
    } else {
      
      if (all(!is.na(strptime(to_date, format = "%Y-%m-%d")))){
        to_date.qtr <- zoo::as.yearqtr(as.Date(to_date))
        to_date <- gsub("\\s", "-", to_date.qtr)
      } else {
        if (all(grepl("^[0-9]{4}.Q[1-4]$", to_date))){
          # Nothing to be done
        } else {
          stop("Emergency stop: to_date could not be identified.")
        }
      }
    }  
  }
  
  
  cpi_inflator_quarters(from_nominal_price = from_nominal_price,
                        from_qtr = from_date,
                        to_qtr = to_date,
                        ...)
}

