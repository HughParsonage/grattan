#' CPI inflator when dates are nice
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_qtr (date in quarters) the dates contemporaneous to the prices in from_nominal_price. Must be of the form "YYYY-Qq" e.g. "1066-Q2". Q1 = Mar, Q2 = Jun, Q3 = Sep, Q4 = Dec.
#' @param to_qtr (date in quarters) the date to be inflated to, where nominal price = real price. Must be of the form "YYYY-Qq" e.g. "1066-Q2".
#' @param adjustment Should there be an adjustment made to the index? Adjustments include 'none' (no adjustment), 'seasonal', or 'trimmed' [referring to trimmed mean]. By default, \code{seasonal}.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' The internal data was updated on 2022-02-03 to 2021-Q3.
#' Using \code{useABSConnection = TRUE} is no longer supported for server issues. 
#' @return A vector of real prices.
#' @export

cpi_inflator_quarters <- function(from_nominal_price,
                                  from_qtr,
                                  to_qtr,
                                  adjustment = c("seasonal", "trimmed", "none"),
                                  useABSConnection = FALSE) {
  adjustment <- match.arg(adjustment)
  
  if (!all(grepl("([0-9]{4}).?(Q[1-4])", from_qtr)) || !all(grepl("([0-9]{4}).?(Q[1-4])", to_qtr))) {
    stop("Dates must be in quarters. e.g. 1066-Q2")
  }
  
  # Ensure date in ABS quarters 1066-Q2 form
  from_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", from_qtr, perl = TRUE)
  to_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", to_qtr, perl = TRUE)
  
  
  cpi.indices <- 
    if (useABSConnection) {
      if (!requireNamespace("rsdmx", quietly = TRUE)) {
        stop("`useABSConnection = TRUE`, yet package:rsdmx is not installed.")  # nocov
      }
      # Importing the cpi data
      cpi.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948"
      cpi.url.seasonal.adjustment <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948"
      cpi.url.trimmed.mean <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948"
      
      url <- 
        switch(adjustment, 
               "none" = cpi.url,
               "seasonal" = cpi.url.seasonal.adjustment, 
               "trimmed" = cpi.url.trimmed.mean)
      
      cpi <- tryCatch({
        rsdmx::readSDMX(url)
        message("Using ABS sdmx connection")
        setDT(as.data.frame(cpi))
      }, 
      error = function(e) {
        message("SDMX failed with error ", e$m,
                "falling back to useABSConnection = FALSE.")
        switch(adjustment, 
               "none" = cpi_unadj, 
               "seasonal" = cpi_seasonal_adjustment, 
               "trimmed" = cpi_trimmed)
      })
    } else {
      switch(adjustment, 
             "none" = cpi_unadj, 
             "seasonal" = cpi_seasonal_adjustment, 
             "trimmed" = cpi_trimmed)
    }
  
  max_obsTime <- max(cpi.indices[["obsTime"]])
  
  if (max(to_qtr) > max_obsTime ||
      max(from_qtr) > max_obsTime) {
    max_qtr <- max(max(to_qtr), max(from_qtr))
    h <- qtrs_ahead(max_obsTime, max_qtr)
    cpi_index_forecast <- 
      gforecast(cpi.indices[["obsValue"]], h = h + 1L) %>%
      .[["mean"]] %>%
      as.double
    
    cpi.indices.new <- 
      setDT(list(obsTime = c(seq.qtr(max_obsTime, h + 2L)[-1L]),
                 obsValue = cpi_index_forecast))
    
    cpi.indices <-
      rbindlist(list(cpi.indices, cpi.indices.new),
                use.names = TRUE)
  }
  
  inflator(from_nominal_price,
           from = from_qtr,
           to = to_qtr,
           inflator_table = cpi.indices,
           index.col = "obsValue", 
           time.col = "obsTime")
}
