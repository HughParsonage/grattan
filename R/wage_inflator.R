#' @title Inflation using the Wage Price Index.
#' @description Predicts what a wage could expect to grow to, between two financial years.
#' 
#' @param wage The amount to be inflated (1 by default).
#' @param from_fy A character vector of the form "2012-13" representing the FY ending that the wage index is to be taken (i.e. Q4 in that year). FY year must be 1996-97 or later.
#' @param to_fy The FY ending that the wage index is to be taken.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' @param allow.projection If set to \code{TRUE} the \code{forecast} package is used to project forward, if required. 
#' @param forecast.series Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals. A fourth option \code{custom} allows manual forecasts to be set.
#' @param forecast.level The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}.
#' @param wage.series If \code{forecast.series = 'custom'}, a \code{data.table} with two variables, \code{fy_year} and \code{r}. 
#' The variable \code{fy_year} consists of all financial years between the last financial year in the (known) wage series and \code{to_fy} \strong{inclusive}.
#' The variable \code{r} consists of rates of wage growth assumed in each \code{fy_year}, which must be 1 in the first year (to connect with the original wage series).
#' @return The wage inflation between the two years.
#' @export

wage_inflator <- function(wage = 1, from_fy, to_fy, useABSConnection = FALSE, allow.projection = TRUE, 
                          forecast.series = c("mean", "upper", "lower", "custom"), 
                          forecast.level = 95, 
                          wage.series = NULL){
  
  # CRAN
  obsTime <- obsValue <- to_index <- from_index <- NULL
  
  if (anyNA(from_fy) || anyNA(to_fy)){
    stop("from_fy and to_fy contain NAs. Filter before applying this function.")
  }
  
  # Avoid vector recycling
  prohibit_vector_recycling(wage, from_fy, to_fy)
  
  if (useABSConnection) {
    wage.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/all?startTime=1997-Q3"
    wages <- rsdmx::readSDMX(wage.url)
    message("Using ABS sdmx connection")
    wage.indices <- as.data.frame(wages) %>%
      as.data.table
  } else {
    # .wages_trend means the wage indices of the trend index
    wage.indices <- copy(wages_trend)
  }
  
  obsDate <- NULL
  wage.indices[, obsYear := as.integer(sub("([12][0-9]{3}).Q([1-4])", "\\1", obsTime, perl = TRUE))]
  wage.indices[, obsQtr  := as.integer(sub("([12][0-9]{3}).Q([1-4])", "\\2", obsTime, perl = TRUE))]
  
  last.full.yr.in.series <- 
    wage.indices %>%
    dplyr::filter(obsQtr == 2L) %>%
    .[["obsYear"]] %>%
    last 
  
  last.quarter.in.series <- 
    wage.indices %>%
    .[["obsQtr"]] %>%
    last 
  
  if (any(from_fy > yr2fy(last.full.yr.in.series))){
    warning("Projection of from_fy terms not yet supported.")
  }
  
  if (!allow.projection && any(to_fy > yr2fy(last.full.yr.in.series))){
    stop("Not all elements of to_fy are in wage index data.")
  }
  # else allow NAs to propagate
  
  # Use forecast::forecast to inflate forward
  forecast.series <- match.arg(forecast.series)
  if (allow.projection && any(to_fy > yr2fy(last.full.yr.in.series)) && forecast.series != "custom"){
    # Number of quarters beyond the data our forecast must reach
    quarters.ahead <- 
      4L * (max(fy2yr(to_fy)) - last.full.yr.in.series) + 2L - last.quarter.in.series
    
    switch(forecast.series, 
           "mean" = {
             forecasts <- 
               gforecast(wage.indices[["obsValue"]], 
                         h = quarters.ahead, 
                         level = forecast.level) %>%
               magrittr::use_series("mean") %>%
               as.numeric 
           }, 
           "upper" = {
             forecasts <- 
               gforecast(wage.indices[["obsValue"]], 
                         h = quarters.ahead, 
                         level = forecast.level) %>%
               magrittr::use_series("upper") %>%
               as.numeric 
           }, 
           "lower" = {
             forecasts <- 
               gforecast(wage.indices[["obsValue"]], 
                         h = quarters.ahead, 
                         level = forecast.level) %>%
               magrittr::use_series("lower") %>%
               as.numeric
           })
    
    obsQtr <- obsYear <- NULL
    wage.indices.new <- 
      data.table(obsQtr   = (seq(last(wage.indices[["obsQtr"]]) + 1, length.out = quarters.ahead, by = 1) %% 4) + 1, 
                 obsValue = forecasts) %>%
      .[, obsYear := last(wage.indices[["obsYear"]]) + cumsum(obsQtr == 1L)]
      
    wage.indices <- rbindlist(list(wage.indices, wage.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  wage.indices %<>%
    .[obsQtr == 2L] %>%
    .[, fy_year := yr2fy(obsYear)] %>%
    .[, list(fy_year, obsValue)]
  
  if (allow.projection && any(to_fy > yr2fy(last.full.yr.in.series)) && forecast.series == "custom"){
    stopifnot(is.data.table(wage.series), 
              all(c("fy_year", "r") %in% names(wage.series)))
    r <- NULL
    
    first.fy.in.wage.series <- min(wage.series[["fy_year"]])
    
    if (first.fy.in.wage.series != yr2fy(last.full.yr.in.series)){
      stop("The first fy in the custom series must be equal to ", yr2fy(last.full.yr.in.series))
    }
    
    if (!(dplyr::near(wage.series[fy_year == first.fy.in.wage.series][["r"]], 0))){
      stop("r must be 0 at fy_year = ", yr2fy(last.full.yr.in.series))
    }
    
    if (any(wage.series[["r"]] > 1)){
      message("Some r > 1 detected. This is unlikely rate of wage growth (r = 0.025 corresponds to 2.5% wage growth).")
    }
    
    last.obsValue.in.series <- tail(wage.indices, 1L)[["obsValue"]]
    
    wage.series[, obsValue := last.obsValue.in.series * cumprod(1 + r)]
    wage.series[fy_year != first.fy.in.wage.series]
    
    wage.indices <- rbindlist(list(wage.indices, 
                                   wage.series[fy_year != first.fy.in.wage.series]), 
                              use.names = TRUE, 
                              fill = TRUE)
  }
  
  input <-
    data.table(wage = wage,
               from_fy = from_fy,
               to_fy = to_fy)
  
  output <- 
    input %>%
    merge(wage.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
          all.x = TRUE) %>%
    setnames("obsValue", "from_index") %>%
    merge(wage.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
          all.x = TRUE) %>%
    setnames("obsValue", "to_index") %>%
    .[, out := wage * (to_index/from_index)]
  
  output[["out"]]
}