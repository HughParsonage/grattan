#' Convenience functions for creating FY
#' 
#' @param yr_ending An integer representing a year.
#' @param fy.yr A string suspected to be a financial year.
#' @param date A string or date for which the financial year is desired.
#' @export

is.fy <- function(fy.yr){
  # Allowed:
  #  2012-13
  #  201213
  # only
  if(grepl("^([12][0-9]{3}).?[0-9]{2}$", fy.yr)){
      # Are the years consecutive?
      (as.integer(gsub("^([12][0-9]{3}).?[0-9]{2}$", "\\1", fy.yr)) + 1) %% 100 == gsub("^[12][0-9]{3}.?([0-9]{2})$", "\\1", fy.yr)
  } else FALSE
}

fy.year <- function(yr_ending){
  paste0(as.integer(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}

yr2fy <- function(...) fy.year(...)

fy2yr <- function(fy.yr){
  if(any(!is.fy(fy.yr))){
    stop("fy.yr contains non-FYs")
  } else  {
    1 + as.integer(gsub("^.*([12][0-9]{3}).?[0-9]{2}.*$", "\\1", fy.yr))
  }
}

fy2date <- function(x){
  if(!any(is.fy(x))){
    stop("fy.yr contains non-FYs")
  } else {
    date <- paste0(as.numeric(gsub("^([1-9][0-9]{3}).*", "\\1", x)) + 1, "-06-30") 
    as.Date(date)
  }
}

date2fy <- function(date){
  assertthat::is.date(date)
  ifelse(lubridate::month(date) < 7, 
         yr2fy(lubridate::year(date)), 
         yr2fy(lubridate::year(date) + 1))
}
