#' CPI inflator
#' 
#' @name cpi_inflator
#' @export 
#' @param from_nominal_price (numeric) the price (or vector of prices) to be inflated
#' @param from_fy (character) a character vector with each element in the form "2012-13" representing the financial year contemporaneous to the from_nominal_price. 
#' @param to_fy (character) a character vector with each element in the form "2012-13" representing the financial year that prices are to be inflated. 
#' @param adjustment What CPI index to use ("none" = raw series, "seasonal", or "trimmed" [mean]).
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' @return the value of from_nominal_price in real (to_fy) dollars.

cpi_inflator <- function(from_nominal_price = 1, from_fy, to_fy = "2014-15", 
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE){
  if (any(is.na(from_fy)) || any(is.na(to_fy))){
    stop("from_fy and to_fy contain NAs. Filter before applying.")
  }
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  prohibit_vector_recycling(from_nominal_price, from_fy, to_fy)
  
  if (useABSConnection) {
    cpi.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10+20.Q/ABS?startTime=1948&endTime=2016"
    cpi.url.seasonal.adjustment <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948&endTime=2016"
    cpi.url.trimmed.mean <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948&endTime=2016"
    
    if (grepl("none", adjustment)){
      url <- cpi.url
    }
    if (grepl("season", adjustment)){
      url <- cpi.url.seasonal.adjustment
    }
    if (grepl("trimmed", adjustment)){
      url <- cpi.url.trimmed.mean
    }
    
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    cpi <- as.data.frame(cpi)
  } else {
    if (grepl("none", adjustment)){
      cpi <- cpi_unadj
    }
    if (grepl("season", adjustment)){
      cpi <- cpi_seasonal_adjustment
    }
    if (grepl("trimmed", adjustment)){
      cpi <- cpi_trimmed
    }
  }
  
  cpi.indices <- 
    data.table::as.data.table(cpi) %>%
    dplyr::filter(grepl("Q1", obsTime)) %>%
    dplyr::mutate(fy_year = yr2fy(sub("-Q1", "", obsTime, fixed = TRUE)))
  
  input <-
    data.table::data.table(from_nominal_price = from_nominal_price,
                           from_fy = from_fy,
                           to_fy = to_fy)
  
  if (!allow.projection && !all(to_fy %in% cpi.indices$fy_year)){
    stop("Not all elements of to_fy are in CPI data.")
  }
  # else allow NAs to propagate
  
  # Use forecast::forecast to inflate forward
  if (allow.projection && !all(to_fy %in% cpi.indices$fy_year)){
    # Number of years beyond the data our forecast must reach
    years.beyond <- max(fy2yr(to_fy)) - max(fy2yr(cpi.indices$fy_year))
    cpi_index_forecast <- cpi.indices %$% forecast::forecast(obsValue, h = years.beyond) %$% as.numeric(mean)
    cpi.indices.new <- 
      data.table::data.table(fy_year = yr2fy(seq(max(fy2yr(cpi.indices$fy_year)) + 1,
                                                 max(fy2yr(to_fy)),
                                                 by = 1L)),
                             obsValue = cpi_index_forecast)
    cpi.indices <- data.table::rbindlist(list(cpi.indices, cpi.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  output <- 
    input %>%
    data.table:::merge.data.table(cpi.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
                                  all.x = TRUE) %>%
    dplyr::rename(from_index = obsValue) %>%
    data.table:::merge.data.table(cpi.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
                                  all.x = TRUE) %>%
    dplyr::rename(to_index = obsValue) %>%
    dplyr::mutate(out = from_nominal_price * (to_index/from_index))
  
  return(output$out)
}
