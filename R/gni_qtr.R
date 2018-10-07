#' Gross National Income, Australia
#' @name gni
#' @description Gross national income, at contemporaneous prices (called 'current prices' by the ABS). 
#' 
#' @param date A Date vector or character coercible thereto.
#' @param fy_year Character vector of financial years.
#' @param roll Passed to \code{data.table} when joining.
#' @return For \code{gni_qtr}, the quarterly GNI for the nearest quarter date. 
#' For \code{gni_fy} the sum over the quarters in the financial year provided. 
#' If \code{fy_year} would provide incomplete data (i.e. only sum three or fewer quarters), a warning is issued.
#' Dates or fy_year outside the available data is neither a warning nor an error, but \code{NA}.
#' @source Australian Bureau of Statistics, Catalogue 5206.0. Series A2304354T.
#' @export gni_qtr gni_fy
NULL

#' @rdname gni
gni_qtr <- function(date, roll = "nearest"){
  # CRAN NOTE avoidance
  ordering <- Date <- Series_ID <- value <- NULL
  
  input <-
    data.table(Date = if (assertthat::is.date(date)) date else as.Date(date)) %>%
    .[, ordering := 1:.N] %>%
    setkeyv("Date") 
  # A2304354T is the Series ID for national income at current prices
  abs_key_aggregates %>%
    .[Series_ID == "A2304354T"] %>%
    setkeyv("Date") %>%
    .[input, roll = roll] %>%
    setorderv("ordering") %>%
    .[["value"]]
}

#' @rdname gni
gni_fy <- function(fy_year){
  # CRAN NOTE avoidance
  ordering <- Date <- Series_ID <- value <- NULL
  
  input <- 
    data.table(fy_year = fy_year) %>%
    .[, ordering := 1:.N] %>%
    setkeyv("fy_year")
  
  output <- 
    abs_key_aggregates %>%
    .[Series_ID == "A2304354T"] %>%
    setkeyv("Date") %>%
    .[, fy_year := date2fy(Date)] %>%
    .[, .(GNI = sum(value), 
          quarters = .N), keyby = "fy_year"] %>%
    .[input] 
    
    # if (any(output[["quarters"]] != 4L)){
    #   warning("Incomplete data for some fy_years. ",
    #           "Dates before 1959 or in the very recent ",
    #           "past will not be reliable.")
    # }
  
    output %>%
      setorderv("ordering") %>%
      .[["GNI"]]
}
