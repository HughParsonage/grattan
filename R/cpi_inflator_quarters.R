#' CPI inflator when dates are nice
#' @param nominal.price (numeric) the nominal prices to be converted to a real price
#' @param nominal.date (date in quarters) the dates contemporaneous to the prices in nominal.price. Must be of the form "YYYY-Qq" e.g. "1066-Q2"
#' @param target.date (date in quarters) the date to be inflated to, where nominal price = real price. Must be of the form "YYYY-Qq" e.g. "1066-Q2"
#' @param adjustment Should there be an adjustment made to the index? Adjustments include 'none' (no adjustment), 'seasonal', or 'trimmed mean'. 
#' 
#' @return a vector of real prices
#' @details this uses the ABS stat SDMX interface, which will form the bulk of the execution time. 


cpi_inflator_quarters <- function(nominal.price, nominal.date, target.date, adjustment = "none"){
  
  if(!grepl("([0-9]{4}).(Q[1-4])", nominal.date) || !grepl("([0-9]{4}).(Q[1-4])", target.date))
    stop("Dates must be in quarters. e.g. 1066-Q2")
  
  # Date in quarters 1066-Q2 form
  nominal.date <- gsub("([0-9]{4}).(Q[1-4])", "\\1-\\2", nominal.date)
  target.date <- gsub("([0-9]{4}).(Q[1-4])", "\\1-\\2", target.date)
  nominal.tbl <- data.table::data.table(nominal.price = nominal.price,
                                        nominal.date = nominal.date, 
                                        target.date = target.date)
  
  
  # Importing the cpi data
  cpi.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948&endTime=2015"
  cpi.url.seasonal.adjustment <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948&endTime=2015"
  cpi.url.trimmed.mean <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948&endTime=2015"
  
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
  cpi <- as.data.frame(cpi)
  
  data.table::setnames(cpi, c("obsTime", "obsValue"), c("nominal.date", "nominal.index"))
  nominal.tbl <- merge(cpi, nominal.tbl)
  
  data.table::setnames(cpi, c("nominal.date", "nominal.index"), c("target.date", "target.index"))
  nominal.tbl <- merge(cpi, nominal.tbl)
  
  nominal.tbl$nominal.price * nominal.tbl$target.index / nominal.tbl$nominal.indexs
}