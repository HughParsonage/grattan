#' CPI for general dates
#' 
#' @param nominal.price (numeric) the nominal prices to be converted to a real price
#' @param nominal.date (character, date-like) the 'date' contemporaneous to \code{nominal price}. The acceptable forms are 'YYYY', 'YYYY-YY' (financial year), 'YYYY-MM-DD'.
#' @param target.date (character, date-like) the date at which the real price is valued (where the nominal price equals the real price). Same forms as for \code{nominal.date}
#' @param ... other arguments passed to \code{cpi_inflator_quarters}
#' 
#' @return a vector of real prices in target.date dollars.

cpi_general_date <- function(nominal.price, nominal.date, target.date, ...){
  # Check the date
  if(all(grepl("^[12][0-9]{3}$", nominal.date))){
    date.type <- "Y"
    
    nominal.date <- paste0(nominal.date, "-Q4")
    message("CPI: Using Q4 for each year")
  }
  
  if(all(grepl("^[12][0-9]{3}.[0-9]{2}$", nominal.date))){
    date.type <- "FY"
    year.ending <- as.numeric(gsub("([12][0-9]{3}).*$", "\\1", nominal.date)) + 1
    nominal.date <- paste0(year.ending, "-Q4")
  }
  
  if(all(!is.na(strptime(nominal.date, format = "%Y-%m-%d")))){
    date.type <- "Date"
    nominal.date.qtr <- zoo::as.yearqtr(as.Date(nominal.date)) + 0.5  # for july
    nominal.date <- gsub("\\s", "-", nominal.date.qtr)
  }
  
  if(all(grepl("^[12][0-9]{3}$", target.date))){
    date.type <- "Y"
    
    target.date <- paste0(target.date, "-Q4")
    message("CPI: Using Q4 for each year")
  }
  
  if(all(grepl("^[12][0-9]{3}.[0-9]{2}$", target.date))){
    date.type <- "FY"
    year.ending <- as.numeric(gsub("([12][0-9]{3}).*$", "\\1", target.date)) + 1
    target.date <- paste0(year.ending, "-Q4")
  }
  
  if(all(!is.na(strptime(target.date, format = "%Y-%m-%d")))){
    date.type <- "Date"
    target.date.qtr <- zoo::as.yearqtr(as.Date(target.date)) + 0.5  # for july
    target.date <- gsub("\\s", "-", target.date.qtr)
  }
  
  cpi_inflator_quarters(nominal.price=nominal.price, nominal.date=nominal.date, target.date=target.date, ...)
}