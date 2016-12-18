#' Gross National Income, Australia
#' @description Gross national income, at contemporaneous prices (called 'current prices' by the ABS). 
#' 
#' @param date A Date vector or character coercible thereto.
#' @param roll Passed to \code{data.table} when joining.
#' @return For \code{gni_qtr}, the quarterly GNI for the nearest quarter date.
#' @export gni_qtr gni_fy

gni_qtr <- function(date, roll = "nearest"){
  input <-
    data.table(Date = if (assertthat::is.date(date)) date else as.Date(date)) %>%
    .[, ordering := 1:.N] %>%
    setkeyv("Date") 
  # A2304354T is the Series ID for national income at current prices
  abs_key_aggregates %>%
    .[Series_ID == "A2304354T"] %>%
    setkeyv("Date") %>%
    .[, orig_date := Date] %>%
    .[input, roll = roll] %>%
    setorderv("ordering") %>%
    .[["value"]]
}

gni_fy <- function(fy_year, roll = "nearest"){
  if (any(!is.fy(fy_year))){
    stop("Invalid fys in fy_year.")
  }
  
  4 * gni_qtr(fy2date(fy_year), roll = roll)
}
