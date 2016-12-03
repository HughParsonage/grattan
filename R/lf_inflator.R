#' Labour force inflators
#' 
#' @name lf_inflator
#' @author Hugh Parsonage and Tim Cameron
#' @rdname lf_inflator
#' @aliases lf_inflator_fy
#' @param labour_force A numeric vector.
#' @param from_date The date of \code{labour_force}.
#' @param to_date Dates as a character vector.
#' @param from_fy Financial year of \code{labour_force}.
#' @param to_fy Financial year for which the labour force is predicted.
#' @param useABSConnection Should an sdmx connection be used to get ABS data?
#' @param allow.projection Logical. Should projections be allowed?
#' @param use.month An integer (corresponding to the output of \code{data.table::month}) representing the month of the series used for the inflation.
#' @param forecast.series Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals.
#' @param forecast.level The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}. 
#' @source ABS Cat 6202.0 \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/6202.0?OpenDocument}.
#' @details \code{lf_inflator} is used on dates. The underlying data series is available every month. 
#' @examples
#' lf_inflator_fy(labour_force = 1, from_fy = "2012-13", to_fy = "2013-14")
#' @return The relative labour force between \code{to_date} and \code{for_date} or \code{to_fy} and \code{from_fy}, multiplied by \code{labour_force}.
#' @export lf_inflator lf_inflator_fy

lf_inflator_fy <- function(labour_force = 1, from_fy = "2012-13", to_fy, 
                           useABSConnection = FALSE, allow.projection = TRUE, 
                           use.month = 1L,
                           forecast.series = c("mean", "upper", "lower"),
                           forecast.level = 95) {
  # CRAN
  obsTime <- NULL; obsValue <- NULL; to_index <- NULL; from_index <- NULL
  # pretty sure this is ok. Reflects  dplyr::mutate(fy_year = date2fy(obsTimeDate))
  obsTimeDate <- NULL
  if (useABSConnection){
    lf.url.trend <- 
      "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978"
    lf <- rsdmx::readSDMX(lf.url.trend)
    lf.indices <- as.data.table(as.data.frame(lf))
  } else {
    lf.indices <- lf_trend
  }
  
  lf.indices[, obsDate := as.Date(sprintf("%s-01", obsTime))]
  last.date.in.series <- last(lf.indices[["obsDate"]])
  last.full.fy.in.series <- 
    lf.indices %>%
    # month from data.table::
    .[month(obsDate) == 6] %>%
    .[["obsDate"]] %>%
    last %>%
    date2fy
  
  if (!allow.projection && any(to_fy > last.full.fy.in.series)){
    stop("Not all elements of to_fy are in labour force data.")
  }
  
  # Use forecast::forecast to inflate forward
  if (allow.projection && to_fy > last.full.fy.in.series){
    # Labour force is monthly
    to_date <- fy2date(to_fy)
    months.ahead <- 
      12L * (year(to_date) - year(last.date.in.series)) + month(to_date) - month(last.date.in.series)
    
    forecast.series <- match.arg(forecast.series)
    switch(forecast.series, 
           "mean" = {
             forecasts <- 
               forecast::forecast(lf.indices[["obsValue"]], 
                                  h = months.ahead, 
                                  level = forecast.level) %>%
               magrittr::use_series("mean") %>%
               as.numeric 
           }, 
           "upper" = {
             forecasts <- 
               forecast::forecast(lf.indices[["obsValue"]], 
                                  h = months.ahead, 
                                  level = forecast.level) %>%
               magrittr::use_series("upper") %>%
               as.numeric 
           }, 
           "lower" = {
             forecasts <- 
               forecast::forecast(lf.indices[["obsValue"]], 
                                  h = months.ahead, 
                                  level = forecast.level) %>%
               magrittr::use_series("lower") %>%
               as.numeric
           })
    
    obsDate <- NULL
    lf.indices.new <- 
      data.table(obsValue = forecasts, 
                 obsDate  = seq.Date(last.date.in.series, to_date, by = "month")[-1]) %>%
      .[, obsTime := sprintf("%s-%02d", year(obsDate), month(obsDate))]

    lf.indices <- rbindlist(list(lf.indices, lf.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  stopifnot(use.month %in% 1:12)
  
  lf.indices <- 
    lf.indices %>%
    .[month(obsDate) == use.month] %>%
    .[, fy_year := date2fy(obsDate)]
  
  
  input <-
    data.table(labour_force = labour_force,
               from_fy = from_fy,
               to_fy = to_fy)
  
  output <- 
    input %>%
    merge(lf.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
          all.x = TRUE) %>%
    setnames("obsValue", "from_index") %>%
    merge(lf.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
          all.x = TRUE) %>%
    setnames("obsValue", "to_index") %>%
    .[, out := labour_force * (to_index/from_index)]

  
  output[["out"]]
}

#' @rdname lf_inflator
#' @examples
#' \dontrun{
#' lf_inflator(labour_force = 1, from_date = "2013-06-30", to_date = "2014-06-30")
#' }
lf_inflator <- function(labour_force = 1, from_date = "2013-06-30", to_date, useABSConnection = FALSE){
  obsTimeDate <- obsTime <- NULL
  # lf original
  if (useABSConnection){
    lf.url <- 
      "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.10.M/ABS?startTime=1981"
    lf.url.trend <- 
      # "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978-02"
      "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978"
    lf <- rsdmx::readSDMX(lf.url.trend)
    lf <- as.data.frame(lf) %>% as.data.table
  } else {
    lf <- lf_trend
  }
  
  lf[ , obsTimeDate := as.Date(paste0(obsTime, "-01"), format = "%Y-%m-%d")]
  
  
  date_connector <- 
    data.table(altkey = seq_along(from_date),
               from_date = as.Date(from_date),
               to_date = as.Date(to_date))
  
  setkeyv(lf, "obsTimeDate")
  
  setkey(date_connector, from_date)
  from_DT <- lf[date_connector, roll=Inf]
  setnames(from_DT, c("obsTimeDate", "obsValue"), c("from_Date", "from_LF"))
  
  setkey(date_connector, to_date)
  to_DT <- lf[date_connector, roll=Inf]
  setnames(to_DT, c("obsTimeDate", "obsValue"), c("to_Date", "to_LF"))
  
  merged <- merge(from_DT, to_DT, by = "altkey")
  labour_force * merged$to_LF / merged$from_LF
}

