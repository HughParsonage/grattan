#' CPI inflator when dates are nice
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_qtr (date in quarters) the dates contemporaneous to the prices in from_nominal_price. Must be of the form "YYYY-Qq" e.g. "1066-Q2". Q1 = Mar, Q2 = Jun, Q3 = Sep, Q4 = Dec.
#' @param to_qtr (date in quarters) the date to be inflated to, where nominal price = real price. Must be of the form "YYYY-Qq" e.g. "1066-Q2".
#' @param adjustment Should there be an adjustment made to the index? Adjustments include 'none' (no adjustment), 'seasonal', or 'trimmed' [referring to trimmed mean]. By default, \code{seasonal}.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' The internal data is up-to-date as of 2018-Q1. 
#' If using \code{useABSConnection = TRUE}, ensure you have \code{rsdmx (>= 0.5-10)} up-to-date.
#' @return A vector of real prices.
#' @export

cpi_inflator_quarters <- function(from_nominal_price,
                                  from_qtr,
                                  to_qtr,
                                  adjustment = c("seasonal", "trimmed", "none"),
                                  useABSConnection = FALSE ){
  adjustment <- match.arg(adjustment)
  
  if (!all(grepl("([0-9]{4}).?(Q[1-4])", from_qtr)) || !all(grepl("([0-9]{4}).?(Q[1-4])", to_qtr))) {
    stop("Dates must be in quarters. e.g. 1066-Q2")
  }
  
  # Ensure date in ABS quarters 1066-Q2 form
  from_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", from_qtr, perl = TRUE)
  to_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", to_qtr, perl = TRUE)
  
  cpi.indices <- 
    if (useABSConnection) {
      # Importing the cpi data
      cpi.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948"
      cpi.url.seasonal.adjustment <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948"
      cpi.url.trimmed.mean <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948"
      
      url <- 
        switch(adjustment, 
               "none" = cpi.url,
               "seasonal" = cpi.url.seasonal.adjustment, 
               "trimmed" = cpi.url.trimmed.mean)
      
      cpi <- rsdmx::readSDMX(url)
      message("Using ABS sdmx connection")
      setDT(as.data.frame(cpi))
    } else {
      switch(adjustment, 
             "none" = cpi_unadj, 
             "seasonal" = cpi_seasonal_adjustment, 
             "trimmed" = cpi_trimmed)
  }
  
  inflator(from_nominal_price,
           from = from_qtr,
           to = to_qtr,
           inflator_table = cpi.indices,
           index.col = "obsValue", 
           time.col = "obsTime")
}
