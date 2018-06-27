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
    {as.integer(sub("^([12][0-9]{3})[-\\s]?[0-9]{2}$", "\\1", fy.yr[potential_fys], perl = TRUE)) + 1L} %% 100L == as.integer(sub("^[12][0-9]{3}[-\\s]?([0-9]{2})$", "\\1", fy.yr[potential_fys], perl = TRUE))
  out
}

is_fy2 <- function(x) {
  a <- c("1977-78", "1978-79", "1979-80", "1980-81", "1981-82", "1982-83", 
         "1983-84", "1984-85", "1985-86", "1986-87", "1987-88", "1988-89", 
         "1989-90", "1990-91", "1991-92", "1992-93", "1993-94", "1994-95", 
         "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01", 
         "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", 
         "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", 
         "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
         "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", 
         "2025-26", "2026-27", "2027-28", "2028-29", "2029-30", "2030-31")
  x %fin% a
}

all_fy <- function(x) {
  a <- c("1977-78", "1978-79", "1979-80", "1980-81", "1981-82", "1982-83", 
         "1983-84", "1984-85", "1985-86", "1986-87", "1987-88", "1988-89", 
         "1989-90", "1990-91", "1991-92", "1992-93", "1993-94", "1994-95", 
         "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01", 
         "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", 
         "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", 
         "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
         "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", 
         "2025-26", "2026-27", "2027-28", "2028-29", "2029-30", "2030-31")
  !anyNA(fmatch(x, a))
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


date2fy <- function(date, back=NULL){
  if(!is.null(back)){
    Year<-year(date) - back
  }else{
    Year<-year(date)
  }
  if_else(month(date) < 7, 
          yr2fy(Year), 
          yr2fy(Year + 1))
}

