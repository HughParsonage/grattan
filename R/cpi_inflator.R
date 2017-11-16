#' CPI inflator
#' 
#' @name cpi_inflator
#' @export 
#' @param from_nominal_price (numeric) the price (or vector of prices) to be inflated
#' @param from_fy (character) a character vector with each element in the form "2012-13" representing the financial year contemporaneous to the from_nominal_price. 
#' @param to_fy (character) a character vector with each element in the form "2012-13" representing the financial year that prices are to be inflated. 
#' @param adjustment What CPI index to use ("none" = raw series, "seasonal", or "trimmed" [mean]).
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date. The internal data was updated on 2017-08-16.
#' 
#' There was a bug in \code{rsdmx 0.5-9} that prevented this working reliably.
#' As this argument is the only to use \code{rsdmx}, version \code{0.5-10} is not a strict
#' dependency, but users would be well-advised to use it. 
#' 
#' @param allow.projection Should projections beyond the ABS's data be allowed?
#' @examples 
#' cpi_inflator(100, from_fy = "2005-06", to_fy = "2014-15")
#' @return The value of \code{from_nominal_price} in real (\code{to_fy}) dollars.

cpi_inflator <- function(from_nominal_price = 1, from_fy, to_fy = "2014-15", 
                         adjustment = c("seasonal", "none", "trimmed.mean"),
                         useABSConnection = FALSE,
                         allow.projection = TRUE){
  # CRAN
  obsTime <- NULL; obsValue <- NULL; to_index <- NULL; from_index <- NULL
  
  if (anyNA(from_fy) || anyNA(to_fy)){
    stop("from_fy and to_fy contain NAs. Remove NAs before applying.")
  }
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  prohibit_vector_recycling(from_nominal_price, from_fy, to_fy)
  
  stopifnot(all(is.fy(from_fy)), all(is.fy(to_fy)))
  
  adjustment <- match.arg(adjustment, several.ok = FALSE)
  
  if (useABSConnection) {
    switch(adjustment, 
           "none" = url <-     
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10.Q/ABS?startTime=1948", 
           
           "seasonal" = url <- 
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948",
           
           "trimmed.mean" = url <- 
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948")
    
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    cpi <- as.data.frame(cpi)
  } else {
    switch(adjustment, 
           "none" = cpi <- cpi_unadj,
           "seasonal" = cpi <- cpi_seasonal_adjustment,
           "trimmed.mean" = cpi <- cpi_trimmed)
  }
  
  cpi.indices <- 
    as.data.table(cpi) %>%
    .[grepl("Q1", obsTime)] %>%
    .[, fy_year := yr2fy(sub("-Q1", "", obsTime, fixed = TRUE))]
  
  input <-
    data.table(from_nominal_price = from_nominal_price,
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
    cpi_index_forecast <- cpi.indices %$% gforecast(obsValue, h = years.beyond) %$% as.numeric(mean)
    cpi.indices.new <- 
      data.table(fy_year = yr2fy(seq(max(fy2yr(cpi.indices$fy_year)) + 1,
                                                 max(fy2yr(to_fy)),
                                                 by = 1L)),
                             obsValue = cpi_index_forecast)
    cpi.indices <- rbindlist(list(cpi.indices, cpi.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  inflator(from_nominal_price, from = from_fy, to = to_fy, inflator_table = cpi.indices,
           index.col = "obsValue", 
           time.col = "fy_year")
}
