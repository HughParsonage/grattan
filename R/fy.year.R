#' Convenience function for creating FY
#' 
#' @param yr_ending ()
#' @value the financial year ("YYYY-YY") corresponding to June of \code{yr_ending}

fy.year <- function(yr_ending){
  paste0(as.numeric(yr_ending) - 1, "-", substr(yr_ending, 3, 4))
}

yr2fy <- function(...) fy.year(...)

fy2yr <- function(fy.yr){
  1 + as.numeric(gsub("^.*([12][0-9]{3}).[0-9]{2}.*$", "\\1", fy.yr))
}