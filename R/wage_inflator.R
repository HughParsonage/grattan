#' @title Inflation using the Wage Price Index.
#' @description Predicts the inflation of hourly rates of pay, between two financial years.
#' 
#' @param wage The amount to be inflated (1 by default).
#' @param from_fy A character vector of the form "2012-13" representing the FY ending that the wage index is to be taken (i.e. Q4 in that year). FY year must be 1996-97 or later.
#' @param to_fy The FY ending that the wage index is to be taken.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date. The internal data was updated on 2018-05-21 to include data up to 2018-Q1.
#' @param allow.projection If set to \code{TRUE} the \code{forecast} package is used to project forward, if required. 
#' @param forecast.series Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals. A fourth option \code{custom} allows manual forecasts to be set.
#' @param forecast.level The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}.
#' @param wage.series If \code{forecast.series = 'custom'}, how future years should be inflated. 
#' The future wage series can be provided in two ways: 
#' (1) a single value, to be the assumed rate of wage inflation in years beyond the known series, or 
#' (2) a \code{data.table} with two variables, \code{fy_year} and \code{r}. If (2), 
#' the variable \code{fy_year} must be a vector of all financial years after the last financial year in the (known) wage series and the latest \code{to_fy} \strong{inclusive}.
#' The variable \code{r} consists of rates of wage growth assumed in each \code{fy_year}.
#' 
#' @examples
#' # Wage inflation
#' wage_inflator(from_fy = "2013-14", to_fy = "2014-15")
#' 
#' # Custom wage inflation
#' wage_inflator(from_fy = "2016-17",
#'               to_fy = "2017-18",
#'               forecast.series = "custom",
#'               wage.series = 0.05)
#' 
#' 
#' @return The wage inflation between the two years.
#' @export

wage_inflator <- function(wage = 1, 
                          from_fy, 
                          to_fy, 
                          useABSConnection = FALSE,
                          allow.projection = TRUE, 
                          forecast.series = c("mean", "upper", "lower", "custom"), 
                          forecast.level = 95, 
                          wage.series = NULL) {
  
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
  
  last_full_yr_in_series <- 
    wage.indices %>%
    .[obsQtr == 2L, .SD, .SDcols = "obsYear"] %>%
    .[["obsYear"]] %>%
    last 
  
  last_full_fy_in_series <- yr2fy(last_full_yr_in_series)
  
  last.quarter.in.series <- 
    wage.indices %>%
    .[["obsQtr"]] %>%
    last 
  
  exponent <- rep_len(1L, length(from_fy))
  if (any(from_fy > to_fy)){
    exponent[from_fy > last_full_fy_in_series] <- -1L
    
    .from <- pmin(from_fy, to_fy)
    .to   <- pmax(to_fy, from_fy)
    
    from_fy <- .from
    to_fy <- .to
  }
  
  if (!allow.projection && any(to_fy > last_full_fy_in_series)){
    stop("Not all elements of to_fy are in wage index data.")
  }
  # else allow NAs to propagate
  
  # Use forecast::forecast to inflate forward
  forecast.series <- match.arg(forecast.series)
  if (AND(allow.projection, 
          AND(any(to_fy > last_full_fy_in_series),
              forecast.series != "custom"))) {
    # Number of quarters beyond the data our forecast must reach
    quarters.ahead <- 
      4L * (max(fy2yr(to_fy)) - last_full_yr_in_series) + 2L - last.quarter.in.series
    
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
      data.table(obsQtr   = (seq(last(wage.indices[["obsQtr"]]),
                                 length.out = quarters.ahead,
                                 by = 1) %% 4) + 1, 
                 obsValue = forecasts) %>%
      .[, obsYear := last(wage.indices[["obsYear"]]) + cumsum(obsQtr == 1L)]
      
    wage.indices <- 
      rbindlist(list(wage.indices, wage.indices.new),
                use.names = TRUE,
                fill = TRUE)
  }
  
  wage.indices %<>%
    .[obsQtr == 2L] %>%
    .[, fy_year := yr2fy(obsYear)] %>%
    .[, list(fy_year, obsValue)]
  
  if (allow.projection && any(to_fy > last_full_fy_in_series) && forecast.series == "custom"){
    if (!is.data.table(wage.series)){
      if (length(wage.series) == 1L){
        years_required <- seq.int(from = last_full_yr_in_series + 1, 
                                  to = fy2yr(max(to_fy)))
        
        wage.series <- data.table(fy_year = yr2fy(years_required), 
                                  r = wage.series)
      } else {
        stop("wage.series must be either a length-one vector", 
             " or a data.table.")
      }
    } else {
      stopifnot(all(c("fy_year", "r") %in% names(wage.series)))
      r <- NULL
      
      
      first_fy_in_wage_series <- min(wage.series[["fy_year"]])
      
      if (first_fy_in_wage_series != yr2fy(last_full_yr_in_series + 1)){
        stop("The first fy in the custom series must be equal to ",
             yr2fy(last_full_yr_in_series + 1))
      }
      
      # Determine whether the dates are a regular sequence (no gaps)
      input_series_fys <- wage.series[["fy_year"]]
      expected_fy_sequence <-
        yr2fy(seq.int(from = last_full_yr_in_series + 1, 
                      to  = last_full_yr_in_series + nrow(wage.series)))
      
      if (!identical(input_series_fys, expected_fy_sequence)){
        stop("wage.series$fy_year should be ", dput(expected_fy_sequence), ".")
      }
    }
    
    if (any(wage.series[["r"]] > 1)){
      message("Some r > 1 in wage.series.",
              "This is unlikely rate of wage growth",
              "(r = 0.025 corresponds to 2.5% wage growth).")
    }
    
    last_obsValue_in_actual_series <- last(wage.indices[["obsValue"]])
    
    wage.series[, obsValue := last_obsValue_in_actual_series * cumprod(1 + r)]
    
    wage.indices <- rbindlist(list(wage.indices, 
                                   wage.series), 
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
    .[, out := wage * (to_index/from_index) ^ exponent]
  
  output[["out"]]
}

