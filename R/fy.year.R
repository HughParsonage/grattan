#' Convenience function for creating FY
#' 
#' @param yr_ending ()
#' @return the financial year ("YYYY-YY") corresponding to June of \code{yr_ending}

is.fy <- function(fy.yr){
  !grepl("^.*([12][0-9]{3}).?[0-9]{2}.*$", fy.yr) |
      # Are the years consecutive?
      ((as.integer(gsub("^.*([12][0-9]{3}).?[0-9]{2}.*$", "\\1", fy.yr)) + 1) %% 100) == as.numeric(gsub("^.*[12][0-9]{3}.?([0-9]{2}).*$", "\\1", fy.yr))
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
