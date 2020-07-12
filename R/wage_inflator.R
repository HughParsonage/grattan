#' @title Inflation using the Wage Price Index.
#' @description Predicts the inflation of hourly rates of pay, between two financial years.
#' 
#' @param wage The amount to be inflated (1 by default).
#' @param from_fy,to_fy (character) a character vector with each element in the form "2012-13" representing the financial years between which the CPI inflator is desired.
#' 
#' If both \code{from_fy} and \code{to_fy} are \code{NULL} (the default), \code{from_fy} is set to the previous financial year and \code{to_fy} to the current financial year, with a warning. Setting only one is an error.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' 
#' If the SDMX connection fails, a message is emitted (not a warning) and
#' the function contines as if \code{useABSConnection = FALSE}.
#' 
#' The internal data was updated on 2020-07-02 to 2020-Q1. 
#' @param allow.projection If set to \code{TRUE} the \code{forecast} package is used to project forward, if required. 
#' @param forecast.series Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals. A fourth option \code{custom} allows manual forecasts to be set.
#' @param forecast.level The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}.
#' @param wage.series If \code{forecast.series = 'custom'}, how future years should be inflated. 
#' The future wage series can be provided in two ways: 
#' (1) a single value, to be the assumed rate of wage inflation in years beyond the known series, or 
#' (2) a \code{data.table} with two variables, \code{fy_year} and \code{r}. If (2), 
#' the variable \code{fy_year} must be a vector of all financial years after the last financial year in the (known) wage series and the latest \code{to_fy} \strong{inclusive}.
#' The variable \code{r} consists of rates of wage growth assumed in each \code{fy_year}.
#' @param accelerate.above An integer setting the threshold for 'acceleration'. 
#' When the maximum length of the arguments exceeds this value, calculate each unique value individually 
#' then combine. Set to 100,000 as a rule of thumb beyond which calculation speeds benefit
#' dramatically. Can be set to \code{Inf} to disable acceleration.
#' 
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
                          from_fy = NULL, 
                          to_fy = NULL, 
                          useABSConnection = FALSE,
                          allow.projection = TRUE, 
                          forecast.series = c("mean", "upper", "lower", "custom"), 
                          forecast.level = 95, 
                          wage.series = NULL,
                          accelerate.above = 1e5L) {
  
  # CRAN
  obsTime <- obsValue <- to_index <- from_index <- NULL
  
  if (is.null(from_fy) && is.null(to_fy)) {
    to_fy <- date2fy(Sys.Date())
    from_fy <- prev_fy(to_fy)
    warning("`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  }
  if (is.null(from_fy)) {
    stop("`from_fy` is missing, with no default.")
  } 
  if (is.null(to_fy)) {
    stop("`to_fy` is missing, with no default.")
  }
  
  check_TF(useABSConnection)
  check_TF(allow.projection)
  
  # Avoid vector recycling
  max.length <- prohibit_vector_recycling.MAXLENGTH(wage, from_fy, to_fy)
  forecast.series <- match.arg(forecast.series)
  
  if (max.length == 1L && !useABSConnection) {
    a <- validate_fys_permitted(from_fy, min.yr = min.wage.yr)
    b <- validate_fys_permitted(to_fy, min.yr = min.wage.yr)
    early_return <- 
      if (a < b) {
        max_fy2yr(b) <= max.wage.yr
      } else {
        max_fy2yr(a) <= max.wage.yr
      }
    if (early_return) {
      if (.getOption("grattan.verbose", FALSE)) {
        cat("\na: ", a, "\t", max_fy2yr(a), "\tb: ", b, "\t", max_fy2yr(b), "\n")
      }
      Values <- .subset2(wages_trend_fy, "obsValue")
      fy_years <- .subset2(wages_trend_fy, "fy_year")
      i <- Values[fmatch(b, fy_years)] / Values[fmatch(a, fy_years)]
      return(wage * i)
    }
  }
  
  if (max.length > accelerate.above && 
      # don't connect for every group
      !useABSConnection &&
      length(wage) == 1L) {
    if (length(to_fy) == 1L) {
      wage_fun <- function(x) {
        wage_inflator(wage = wage[[1L]],
                      from_fy = x, 
                      to_fy = to_fy[[1L]],
                      forecast.series = forecast.series[[1L]], 
                      useABSConnection = FALSE,
                      wage.series = wage.series,
                      allow.projection = allow.projection[[1L]],
                      accelerate.above = Inf)
      }
      return(accel_repetitive_input(from_fy, wage_fun))
    }
    if (length(from_fy) == 1L) {
      wage_fun <- function(x) {
        wage_inflator(from_fy = from_fy[[1L]], 
                     to_fy = x,
                     forecast.series = forecast.series[[1L]], 
                     useABSConnection = FALSE,
                     wage.series = wage.series,
                     allow.projection = allow.projection[[1L]],
                     accelerate.above = Inf)
      }
      return(accel_repetitive_input(to_fy, wage_fun))
    }
  }
  
  if (useABSConnection) {
    # nocov start
    if (!requireNamespace("rsdmx", quietly = TRUE)) {
      stop("`useABSConnection = TRUE`, yet package:rsdmx is not installed.")  # nocov
    }
    wage.url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/all?startTime=1997-Q3"
    tryCatch({
      wages <- rsdmx::readSDMX(wage.url)
      message("Using ABS sdmx connection")
      wage.indices <- setDT(as.data.frame(wages))
      split2yq <- function(x) {
        lapply(tstrsplit(x, split = ".Q", perl = TRUE),
               as.integer)
      }
      wage.indices[, c("obsYear", "obsQtr") := split2yq(obsTime)]
      min.wage.yr <- wage.indices[, min(obsYear)]
    },
    error = function(e) {
      message("SDMX failed with error ", e$m,
              "falling back to useABSConnection = FALSE.")
      wage.indices <- copy(wages_trend)
    })
    # nocov end
  } else {
    # .wages_trend means the wage indices of the trend index
    wage.indices <- copy(wages_trend)
  }
  
  obsYear <- obsQtr <- NULL
  last_full_yr_in_series <- 
    wage.indices[obsQtr == 2L, last(obsYear)]
  
  last_full_fy_in_series <- yr2fy(last_full_yr_in_series)
  
  last.quarter.in.series <- 
    wage.indices[, last(obsQtr)]
  
  from_fy <- validate_fys_permitted(from_fy, min.yr = min.wage.yr)
  to_fy <- validate_fys_permitted(to_fy, min.yr = min.wage.yr)
  
  if (max.length == 1L ||
      AND(length(from_fy) == 1L, 
          length(to_fy) == 1L)) {
    exponent <- if (from_fy > to_fy) -1L else 1L
  } else {
    exponent <- rep_len(1L, length(from_fy))
    if (any(are_deflator <- from_fy > to_fy)){
      exponent[are_deflator] <- -1L
      
      .from <- pmin(from_fy, to_fy)
      .to   <- pmax(to_fy, from_fy)
      
      from_fy <- .from
      to_fy <- .to
    }
  }
  
  max_to_yr <- max_yr <- max_fy2yr(to_fy)
  
  if (!allow.projection && max_yr > last_full_yr_in_series) {
    stop("Not all elements of to_fy are in wage index data.")
  }
  # else allow NAs to propagate
  
  # Use forecast::forecast to inflate forward
  
  if (AND(allow.projection, 
          AND(max_yr > last_full_yr_in_series,
              forecast.series != "custom"))) {
    # Number of quarters beyond the data our forecast must reach
    quarters.ahead <- 
      4L * (max_yr - last_full_yr_in_series) + 2L - last.quarter.in.series
    
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
  
  if (allow.projection &&
      max_yr > last_full_yr_in_series &&
      forecast.series == "custom") {
    
    wage.indices <- append_custom_series(orig = wage.indices,
                                         custom.series = wage.series,
                                         max_to_yr = max_to_yr,
                                         last_full_yr_in_orig = last_full_yr_in_series,
                                         last_full_fy_in_orig = last_full_fy_in_series)
  }
  
  infl_factor <-
    inflator(1, 
             from = from_fy,
             to = to_fy,
             inflator_table = wage.indices,
             index.col = "obsValue", 
             time.col = "fy_year",
             max.length = max.length) 
  wage * {infl_factor ^ exponent}
}

