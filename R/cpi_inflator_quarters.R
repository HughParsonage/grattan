#' CPI inflator when dates are nice
#' @param from_nominal_price (numeric) the nominal prices to be converted to a real price
#' @param from_qtr (date in quarters) the dates contemporaneous to the prices in from_nominal_price. Must be of the form "YYYY-Qq" e.g. "1066-Q2". Q1 = Mar, Q2 = Jun, Q3 = Sep, Q4 = Dec.
#' @param to_qtr (date in quarters) the date to be inflated to, where nominal price = real price. Must be of the form "YYYY-Qq" e.g. "1066-Q2".
#' @param adjustment Should there be an adjustment made to the index? Adjustments include 'none' (no adjustment), 'seasonal', or 'trimmed mean'. 
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' @return A vector of real prices.
#' @export

cpi_inflator_quarters <- function(from_nominal_price, from_qtr, to_qtr, adjustment = "seasonal", useABSConnection = FALSE){
  
  if(!all(grepl("([0-9]{4}).?(Q[1-4])", from_qtr)) || !all(grepl("([0-9]{4}).?(Q[1-4])", to_qtr)))
    stop("Dates must be in quarters. e.g. 1066-Q2")
  
  # Ensure date in ABS quarters 1066-Q2 form
  from_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", from_qtr)
  to_qtr <- gsub("([0-9]{4}).?(Q[1-4])", "\\1-\\2", to_qtr)
  
  input <-
    data.table(from_nominal_price = from_nominal_price,
               from_qtr = from_qtr,
               to_qtr = to_qtr)
  
  if (useABSConnection){
    # Importing the cpi data
    cpi.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948"
    cpi.url.seasonal.adjustment <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948"
    cpi.url.trimmed.mean <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948"
    
    if(grepl("none", adjustment)){
      url <- cpi.url
    }
    if(grepl("season", adjustment)){
      url <- cpi.url.seasonal.adjustment
    }
    if(grepl("trimmed", adjustment)){
      url <- cpi.url.trimmed.mean
    }
    
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    cpi.indices <- as.data.frame(cpi) %>% as.data.table
  } else {
    switch(adjustment, 
           "none" = cpi.indices <- cpi_unadj, 
           "seasonal" = cpi.indices <- cpi_seasonal_adjustment, 
           "trimmed" = cpi.indices <- cpi_trimmed)
  }
  
  output <- 
    input %>%
    merge(cpi.indices, by.x = "from_qtr", by.y = "obsTime", sort = FALSE,
          all.x = TRUE) %>%
    setnames("obsValue", "from_index") %>%
    merge(cpi.indices, by.x = "to_qtr", by.y = "obsTime", sort = FALSE, 
          all.x = TRUE) %>%
    setnames("obsValue", "to_index") %>%
    inflator_frac("from_nominal_price", "to_index", "from_index", "out")
  
  output[["out"]]
}