#' Convenience functions for dealing with financial years
#' 
#' @description From grattan v1.7.1.4, these are reexports from the \code{\link[fy]{fy-package}}.
#' 
#' @name is.fy
#' @aliases fy.year yr2fy fy2yr fy2date date2fy
#' @param yr_ending An integer representing a year.
#' @param fy.yr A string suspected to be a financial year.
#' @param date A string or date for which the financial year is desired. Note that \code{yr2fy} does not check its argument is an integer.
#' @param assume1901_2100 For \code{yr2fy}, assume that \code{yr_ending} is between 1901 and 2100, 
#' for performance. By default, set to \code{getOption("grattan.assume1901_2100", TRUE)}.
#' @details The following forms are permitted: \code{2012-13}, \code{201213}, \code{2012 13}, only.
#' However, the \code{2012-13} form is preferred and will improve performance. 
#' 
#' @return For \code{is.fy}, a logical, whether its argument is a financial year.
#' The following forms are allowed: \code{2012-13}, \code{201213}, \code{2012 13}, only.
#' For \code{fy.year}, \code{yr2fy}, and \code{date2fy}, the financial year. 
#' For the inverses, a numeric corresponding to the year.
#' 
#' \code{fy.year} is a deprecated alias for \code{yr2fy}, the latter is slightly more efficient, as well as more declarative.
#' 
#' \code{fy2yr} converts a financial year to the year ending: \code{fy2yr("2016-17")} returns 2017. \code{yr2fy} is the inverse: \code{yr2fy(fy2yr("2016-17")) == "2016-17"}.
#' 
#' \code{fy2date} converts a financial year to the 30 June of the financial year ending.
#' 
#' \code{date2fy} converts a date to the corresponding financial year.
#' 
#' @importFrom fy validate_fys_permitted
#' 
#' @examples
#' is.fy("2012-13")
#' is.fy("2012-14")
#' yr2fy(2012)
#' fy2yr("2015-16")
#' date2fy("2014-08-09")
#' @export is.fy fy.year yr2fy fy2yr fy2date date2fy 
NULL

is.fy <- fy::is_fy

fy.year <- function(yr_ending) {
  paste0(as.integer(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}

yr2fy <- function(...) as.character(fy::yr2fy(...))
fy2yr <- fy::fy2yr
fy2date <- fy::fy2date
date2fy <- function(...) as.character(fy::date2fy(...))
qtr2fy <- function(...) as.character(fy::qtr2fy(...))


max_fy2yr <- function(x) fy2yr(max(x))
min_fy2yr <- function(x) fy2yr(min(x))

all_fy <- function(x, permitted = NULL) {
  if (is.null(permitted)) {
    all(fy::is_fy(x), na.rm = TRUE)
  } else {
    !anyNA(fmatch(x, permitted))
  }
}

is_fy2 <- fy::is_fy


