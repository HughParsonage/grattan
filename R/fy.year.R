#' Convenience functions for dealing with financial years
#' 
#' @name is.fy
#' @aliases fy.year yr2fy fy2yr fy2date date2fy
#' @param yr_ending An integer representing a year.
#' @param fy.yr A string suspected to be a financial year.
#' @param date A string or date for which the financial year is desired. Note that \code{yr2fy} does not check its argument is an integer.
#' @return For \code{is.fy}, a logical, whether its argument is a financial year.
#' The following forms are allowed: \code{2012-13}, \code{201213}, \code{2012 13}, only.
#' For \code{fy.year}, \code{yr2fy}, and \code{date2fy}, the financial year. 
#' For the inverses, a numeric corresponding to the year.
#' @examples
#' is.fy("2012-13")
#' is.fy("2012-14")
#' yr2fy(2012)
#' fy2yr("2015-16")
#' date2fy("2014-08-09")
#' @export is.fy fy.year yr2fy fy2yr fy2date date2fy
NULL



is.fy <- function(fy.yr){
  out <- logical(length(fy.yr))
  potential_fys <- grepl("^([12][0-9]{3})[-\\s]?[0-9]{2}$", fy.yr, perl = TRUE)
  out[potential_fys] <- 
    (as.double(sub("^([12][0-9]{3})[-\\s]?[0-9]{2}$", "\\1", fy.yr[potential_fys], perl = TRUE)) + 1) %% 100 == as.numeric(sub("^[12][0-9]{3}[-\\s]?([0-9]{2})$", "\\1", fy.yr[potential_fys], perl = TRUE))
  out
}


fy.year <- function(yr_ending){
  paste0(as.integer(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}




yr2fy <- function(yr_ending){
  paste0(as.integer(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}



fy2yr <- function(fy.yr){
  if (any(!is.fy(fy.yr))){
    stop("fy.yr contains non-FYs")
  } else  {
    1 + as.integer(gsub("^.*([12][0-9]{3}).?[0-9]{2}.*$", "\\1", fy.yr))
  }
}



fy2date <- function(x){
  if (!any(is.fy(x))){
    stop("fy.yr contains non-FYs")
  } else {
    date <- paste0(as.numeric(gsub("^([1-9][0-9]{3}).*", "\\1", x)) + 1, "-06-30") 
    as.Date(date)
  }
}



date2fy <- function(date){
  if_else(month(date) < 7, 
          yr2fy(year(date)), 
          yr2fy(year(date) + 1))
}
