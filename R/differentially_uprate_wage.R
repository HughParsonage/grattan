#' Differential uprating
#' @description Apply differential uprating to projections of the \code{Sw_amt} variable.
#' @author Hugh Parsonage and William Young
#' @param wage A numeric vector to be uprated.
#' @param from The financial year contemporaneous to wage, which must be a financial year of an available sample file -- in particular, not after 2016-17.
#' @param to The target of the uprating. Passed to \code{\link{wage_inflator}}.
#' @param ... Other arguments passed \code{\link{wage_inflator}}.
#' @return The vector \code{wage} differentially uprated to \code{to_fy}.
#' @details See \code{vignette("differential-uprating")}.
#' @examples 
#' ws <- c(20e3, 50e3, 100e3)
#' from <- "2013-14"
#' to <- "2016-17"
#' differentially_uprate_wage(ws, from, to)
#' differentially_uprate_wage(ws, from, to) / (ws * wage_inflator(from, to))
#' 
#' @export


differentially_uprate_wage <- function(wage = 1, from, to, ...){
  from_fy <- from
  to_fy <- to
  stopifnot(all(from_fy %in% c("2003-04", "2004-05", "2005-06", 
                               "2006-07", "2007-08", "2008-09", 
                               "2009-10", "2010-11", "2011-12", 
                               "2012-13", "2013-14", "2014-15",
                               "2015-16", "2016-17")))
  input <- 
    data.table(fy.year = from_fy, 
               wage = wage) %>%
    # rolling merge will destroy order
    .[, `_order` := 1:.N] %>%
    setkeyv(cols = c("fy.year", "wage"))
  
  `_order` <- NULL
  differential_sw_uprates %>%
    assertthat::has_name(., "uprate_factor") %>%
    assertthat::validate_that(.)
  uprate_factor <- NULL
  `_out` <- NULL
  
  differential_sw_uprates[salary_by_fy_swtile, on = "Sw_amt_percentile"] %>%
    setnames(old = "min_salary", new = "wage") %>%
    setkeyv(cols = c("fy.year", "wage")) %>%
    .[input, roll = "nearest"] %>%
    .[, `_out` := wage * (uprate_factor * (wage_inflator(from = from_fy, to = to_fy, ...) - 1) + 1)] %>%
    setkeyv("_order") %>%
    .[["_out"]]
}
