#' CPI inflator
#' 
#' @name cpi_inflator
#' @export true
#' @param from_nominal_price (numeric) the price (or vector of prices) to be inflated
#' @param from_fy (character) a character vector with each element in the form "2012-13" representing the financial year contemporaneous to the from_nominal_price. 
#' @param to_fy (character) a character vector with each element in the form "2012-13" representing the financial year that prices are to be inflated. 
#' @return the value of from_nominal_price in real (to_fy) dollars.

cpi_inflator <- function(from_nominal_price = 1, from_fy, to_fy = "2013-14", adjustment = "none"){
  if((!require(dplyr)) || (!require(magrittr)))
    stop("dplyr and magrittr required")
  
  cpi.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948&endTime=2016"
  cpi.url.seasonal.adjustment <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948&endTime=2016"
  cpi.url.trimmed.mean <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948&endTime=2016"
  
  if(grepl("none", adjustment)){
    url <- cpi.url
  }
  if(grepl("season", adjustment)){
    url <- cpi.url.seasonal.adjustment
  }
  if(grepl("trimmed", adjustment)){
    url <- cpi.url.trimmed.mean
  }
  
  
  if(require(rsdmx)) {
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    cpi <- as.data.frame(cpi)
  } else {
    #wages <- structure(list(obsTime = c("1997-Q3", "1997-Q4", "1998-Q1", "1998-Q2","1998-Q3", "1998-Q4", "1999-Q1", "1999-Q2", "1999-Q3", "1999-Q4", "2000-Q1", "2000-Q2", "2000-Q3", "2000-Q4", "2001-Q1", "2001-Q2", "2001-Q3", "2001-Q4", "2002-Q1", "2002-Q2", "2002-Q3", "2002-Q4", "2003-Q1", "2003-Q2", "2003-Q3", "2003-Q4", "2004-Q1", "2004-Q2", "2004-Q3", "2004-Q4", "2005-Q1", "2005-Q2", "2005-Q3", "2005-Q4", "2006-Q1", "2006-Q2", "2006-Q3", "2006-Q4", "2007-Q1", "2007-Q2", "2007-Q3", "2007-Q4", "2008-Q1", "2008-Q2", "2008-Q3", "2008-Q4", "2009-Q1", "2009-Q2", "2009-Q3", "2009-Q4", "2010-Q1", "2010-Q2", "2010-Q3", "2010-Q4", "2011-Q1", "2011-Q2", "2011-Q3", "2011-Q4", "2012-Q1", "2012-Q2", "2012-Q3", "2012-Q4", "2013-Q1", "2013-Q2", "2013-Q3", "2013-Q4", "2014-Q1", "2014-Q2", "2014-Q3", "2014-Q4", "2015-Q1"), obsValue = c(66.7, 67.2, 67.8, 68.3, 68.8, 69.4, 69.9, 70.4, 70.9, 71.4, 71.9, 72.5, 73.1, 73.8, 74.5, 75.1, 75.7, 76.3, 76.9, 77.5, 78.2, 78.9, 79.6, 80.3, 81.1, 81.8, 82.5, 83.2, 83.9, 84.8, 85.7, 86.5, 87.4, 88.3, 89.2, 90.1, 91, 91.9, 92.8, 93.7, 94.7, 95.6, 96.6, 97.7, 98.7, 99.7, 100.6, 101.4, 101.8, 102.6, 103.5, 104.5, 105.5, 106.6, 107.5, 108.5, 109.4, 110.4, 111.5, 112.4, 113.4, 114.2, 115, 115.7, 116.5, 117.2, 118, 118.7, 119.4, 120.1, 120.7)), row.names = c(NA, 71L), class = "data.frame", .Names = c("obsTime", "obsValue"))
    warning("No data without rsdmx")
  }
  
  # maps 2013-14 to 2014
  from_fy_year <- 1 + as.numeric(gsub("^.*([12][0-9]{3}).*$", "\\1", from_fy)) 
  from_fy_as_quarter <- paste0(from_fy_year, "-", "Q4")
  
  to_fy_year <- 1 + as.numeric(gsub("^.*([12][0-9]{3}).*$", "\\1", to_fy)) 
  to_fy_as_quarter <- paste0(to_fy_year, "-", "Q4")
  
  if(any(!(from_fy_as_quarter %in% cpi$obsTime)))
    stop("From date not in ABS CPI data.")
  if(any(!(to_fy_as_quarter %in% cpi$obsTime)))
    stop("To date not in ABS CPI data.")
  
  if(length(from_nominal_price) > 1){
    
    temp.df <- dplyr::mutate(cpi[grepl("Q4$", cpi$obsTime), ], 
                             fy.year.start = as.numeric(gsub(".Q[1-4]$", "", obsTime)),
                             fy.year.end = substr(fy.year.start + 1, 3, 4),
                             fy.year = paste0(fy.year.start, "-", fy.year.end))
    
    cpi.when.to_fy <- filter(temp.df, fy.year == to_fy) %>% 
      mutate(to_index = obsValue) %$%
      to_index
    
    data.frame(from_nominal_price = from_nominal_price,
               from_fy = from_fy, 
               stringsAsFactors = FALSE) %>%
      dplyr::left_join(temp.df, by = c("from_fy" = "fy.year")) %>%
      dplyr::mutate(from_index = obsValue) %>%
      dplyr::mutate(to_index = cpi.when.to_fy) %>%
      dplyr::mutate(inflator = from_nominal_price * to_index/obsValue) %$%
      return(inflator)
  } else { 

  
  price * 
    (cpi[cpi$obsTime == to_fy_as_quarter, ]$obsValue / 
      cpi[cpi$obsTime == from_fy_as_quarter, ]$obsValue)
  }
}