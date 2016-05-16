#' CPI inflator
#' 
#' @name cpi_inflator
#' @export 
#' @param from_nominal_price (numeric) the price (or vector of prices) to be inflated
#' @param from_fy (character) a character vector with each element in the form "2012-13" representing the financial year contemporaneous to the from_nominal_price. 
#' @param to_fy (character) a character vector with each element in the form "2012-13" representing the financial year that prices are to be inflated. 
#' @param adjustment What CPI index to use ("none" = raw series, "seasonal", or "trimmed" [mean]).
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' @param allow.projection Should projections beyond the ABS's data be allowed?
#' @return the value of from_nominal_price in real (to_fy) dollars.

cpi_inflator <- function(from_nominal_price = 1, from_fy, to_fy = "2014-15", 
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE){
  # CRAN
  obsTime <- NULL; obsValue <- NULL; to_index <- NULL; from_index <- NULL
  
  if (any(is.na(from_fy)) || any(is.na(to_fy))){
    stop("from_fy and to_fy contain NAs. Remove NAs before applying.")
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
  
  # NSE
  hugh_frac <- function(.data, front, over, under, new_col_name){
    # http://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
    mutate_call <- lazyeval::interp(~r*a/b, a = as.name(over), b = as.name(under), r = as.name(front))
    .data %>%
      dplyr::mutate_(.dots = stats::setNames(list(mutate_call), new_col_name))
  }
  
  output <- 
    input %>%
    merge(cpi.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
                                  all.x = TRUE) %>%
    data.table::setnames("obsValue", "from_index") %>%
    merge(cpi.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
                                  all.x = TRUE) %>%
    data.table::setnames("obsValue", "to_index") %>%
    hugh_frac("from_nominal_price", "to_index", "from_index", "out")
  
  return(output$out)
}
