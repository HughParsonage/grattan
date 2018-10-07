#' Gross Domestic Product, Australia
#' @name gdp
#' @description Gross domestic product, at contemporaneous prices (called \sQuote{current prices} by the ABS). 
#' 
#' @param date A Date vector or character coercible thereto.
#' @param fy_year Character vector of financial years.
#' @param roll Passed to \code{data.table} when joining.
#' @return For \code{gdp_qtr}, the quarterly GDP for the quarter date nearest (or otherwise using \code{roll}). 
#' For \code{gdp_fy} the sum over the quarters in the financial year provided. 
#' If \code{fy_year} would provide incomplete data (i.e. only sum three or fewer quarters), a warning is issued.
#' Dates or fy_year outside the available data is neither a warning nor an error, but \code{NA}.
#' @source Australian Bureau of Statistics, Catalogue 5206.0. Series A2304350J.
#' @export gdp_qtr gdp_fy
NULL

#' @rdname gdp
gdp_qtr <- function(date, roll = "nearest"){
  # CRAN NOTE avoidance
  ordering <- Date <- Series_ID <- value <- NULL
  
  input <-
    data.table(Date = if (assertthat::is.date(date)) date else as.Date(date)) %>%
    .[, ordering := .I] %>%
    setkeyv("Date") 
  # A2304350J is the Series ID for Gross domestic product: Current prices
  abs_key_aggregates %>%
    .[Series_ID == "A2304350J"] %>%
    setkeyv("Date") %>%
    .[input, roll = roll] %>%
    setorderv("ordering") %>%
    .[["value"]]
}

#' @rdname gdp
gdp_fy <- function(fy_year){
  # CRAN NOTE avoidance
  ordering <- Date <- Series_ID <- value <- NULL
  
  input <- 
    data.table(fy_year = fy_year) %>%
    .[, ordering := 1:.N] %>%
    setkeyv("fy_year")
  
  output <- 
    abs_key_aggregates %>%
    .[Series_ID == "A2304350J"] %>%
    setkeyv("Date") %>%
    .[, fy_year := date2fy(Date)] %>%
    .[, .(GNI = sum(value), 
          quarters = .N), keyby = "fy_year"] %>%
    .[input] 
  
  # if (any(output[!is.na(quarters)][["quarters"]] != 4L)){
  #   warning("Incomplete data for some fy_years. Dates before 1959 or in the very recent past will not be reliable.")
  #   set(output, i = which(output[["quarters"]] < 4), j = which(names(output) == "GNI"), value = 4 * output[["GNI"]])
  # }
  
  output %>%
    setorderv("ordering") %>%
    .[["GNI"]]
}
