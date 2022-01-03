#' Labour force inflators
#' 
#' @name lf_inflator
#' @description This function's behaviour has changed due to COVID-19. In particular,
#' the trend labour force status is no longer available.
#' 
#' @author Tim Cameron, Matthew Katzen, and Hugh Parsonage 
#' @rdname lf_inflator
#' @aliases lf_inflator_fy
#' @param labour_force A numeric vector.
#' @param from_date The date of \code{labour_force}. 
#' @param to_date Dates as a character vector.
#' @param from_fy,to_fy (character) a character vector with each element in the form "2012-13" representing the financial years between which the labour force inflator is desired.
#' 
#' If both \code{from_fy} and \code{to_fy} are \code{NULL} (the default), \code{from_fy} is set to the previous financial year and \code{to_fy} to the current financial year, with a warning. Setting only one is an error.
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' 
#' If the SDMX connection fails, a message is emitted (not a warning) and
#' the function contines as if \code{useABSConnection = FALSE}.
#' 
#' The internal data was updated on 2022-01-03 to 2021-11-01.
#' @param allow.projection Logical. Should projections be allowed?
#' @param use.month An integer (corresponding to the output of \code{data.table::month}) representing the month of the series used for the inflation.
#' @param forecast.series Whether to use the forecast mean, or the upper or lower boundaries of the prediction intervals.
#' @param forecast.level The prediction interval to be used if \code{forecast.series} is \code{upper} or \code{lower}. 
#' @param lf.series If \code{forecast.series = 'custom'}, a \code{data.table} with two variables, \code{fy_year} and \code{r}. 
#' The variable \code{fy_year} consists of all financial years between the last financial year in the (known) labour force series and \code{to_fy} \strong{inclusive}.
#' The variable \code{r} consists of rates of labour force growth assumed in each \code{fy_year}, which must be 1 in the first year (to connect with the original labour force series).
#' 
#' @param .lf_indices (Internal use only.) A \code{data.table} sent directly to \code{inflator} without any checks.
#' @param accelerate.above An integer setting the threshold for 'acceleration'. 
#' When the maximum length of the arguments exceeds this value, calculate each unique value individually 
#' then combine. Set to 100,000 as a rule of thumb beyond which calculation speeds benefit
#' dramatically. Can be set to \code{Inf} to disable acceleration.
#' 
#'   
#' @source ABS Cat 6202.0 \url{https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release}.
#' @details \code{lf_inflator} is used on dates. The underlying data series is available every month. 
#' @examples
#' lf_inflator_fy(labour_force = 1, from_fy = "2012-13", to_fy = "2013-14")
#' 
#' library(data.table)
#' # Custom 1% growth over 2018-19 -> 2019-20
#' lf_inflator_fy(from_fy = "2018-19",
#'                to_fy = "2019-20",
#'                forecast.series = "custom", 
#'                lf.series = data.table(fy_year = c("2018-19", "2019-20"),
#'                                       r = c(0, 0.01)))
#' @return The relative labour force between \code{to_date} and \code{for_date} 
#' or \code{to_fy} and \code{from_fy}, multiplied by \code{labour_force}.
#' 
#' @export lf_inflator lf_inflator_fy

lf_inflator_fy <- function(labour_force = 1,
                           from_fy = NULL,
                           to_fy = NULL, 
                           useABSConnection = FALSE,
                           allow.projection = TRUE, 
                           use.month = 1L,
                           forecast.series = c("mean", "upper", "lower", "custom"),
                           forecast.level = 95, 
                           lf.series = NULL,
                           .lf_indices = NULL,
                           accelerate.above = 1e5L) {
  if (!is.null(.lf_indices)) {
    return(inflator(labour_force, 
                    from = from_fy, 
                    to = to_fy, 
                    inflator_table = .lf_indices, 
                    index.col = "obsValue",
                    time.col = "fy_year"))
  }
  
  # CRAN
  obsTime <- obsValue <- NULL
  
  if (is.null(from_fy) && is.null(to_fy)){
    to_fy <- date2fy(Sys.Date())
    from_fy <- prev_fy(to_fy)
    warning("`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  }
  if (is.null(from_fy)){
    stop("`from_fy` is missing, with no default.")
  } 
  if (is.null(to_fy)){
    stop("`to_fy` is missing, with no default.")
  }
  
  check_TF(useABSConnection)
  check_TF(allow.projection)
  
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(labour_force, from_fy, to_fy)
  
  if (max.length == 1L &&
      !useABSConnection &&
      identical(use.month, 1L)) {
    a <- validate_fys_permitted(from_fy, min.yr = min.lf.yr)
    b <- validate_fys_permitted(to_fy, min.yr = min.lf.yr)
    early_return <- 
      if (a < b) {
        max_fy2yr(b) <= max.lf.yr
      } else {
        max_fy2yr(a) <= max.lf.yr
      }
    if (early_return) {
      if (.getOption("grattan.verbose", FALSE)) {
        cat("\na: ", a, "\t", max_fy2yr(a), "\tb: ", b, "\t", max_fy2yr(b), "\n")
      }
      Values <- .subset2(lf_trend_fy, "obsValue")
      fy_years <- .subset2(lf_trend_fy, "fy_year")
      i <- Values[fmatch(b, fy_years)] / Values[fmatch(a, fy_years)]
      return(labour_force * i)
    }
  }
  
  stopifnot(use.month %between% c(1L, 12L))
  
  if (max.length > accelerate.above && 
      # don't connect for every group
      !useABSConnection &&
      length(labour_force) == 1L) {
    if (length(to_fy) == 1L) {
      lf_fun <- function(x) {
        lf_inflator_fy(labour_force = labour_force[[1L]],
                       from_fy = x, 
                       to_fy = to_fy[[1L]],
                       forecast.series = forecast.series[[1L]], 
                       useABSConnection = FALSE,
                       lf.series = lf.series,
                       use.month = use.month,
                       allow.projection = allow.projection[[1L]],
                       accelerate.above = Inf)
      }
      return(accel_repetitive_input(from_fy, lf_fun))
    }
    if (length(from_fy) == 1L) {
      lf_fun <- function(x) {
        lf_inflator_fy(labour_force = labour_force[[1L]],
                       from_fy = from_fy[[1L]], 
                       to_fy = x,
                       forecast.series = forecast.series[[1L]], 
                       useABSConnection = FALSE,
                       lf.series = lf.series,
                       use.month = use.month,
                       allow.projection = allow.projection[[1L]],
                       accelerate.above = Inf)
      }
      return(accel_repetitive_input(to_fy, lf_fun))
    }
  }
  
  from_fy <- validate_fys_permitted(from_fy, min.yr = 1978L)
  to_fy <- validate_fys_permitted(to_fy, min.yr = 1978L)
  
  if (useABSConnection) {
    # nocov start
    if (!requireNamespace("rsdmx", quietly = TRUE)) {
      stop("`useABSConnection = TRUE`, yet package:rsdmx is not installed.")  # nocov
    }
    lf.indices <- 
      tryCatch({
        lf.url.trend <- 
          "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1978"
        lf <- rsdmx::readSDMX(lf.url.trend)
        lf.indices <- as.data.frame(lf)
        lf.indices <- as.data.table(lf.indices)
      },
      error = function(e) {
        message("SDMX failed with error ", e$m,
                "falling back to useABSConnection = FALSE.")
        lf.indices <- as.data.table(lf_trend)
      })
    # nocov end
  } else {
    lf.indices <- as.data.table(lf_trend)
  }
  
  lf.indices[, obsDate := as.Date(sprintf("%s-01", obsTime))]
  last.date.in.series <- last(lf.indices[["obsDate"]])
  
  obsDate <- NULL
  last_full_yr_in_series <- 
    lf.indices %>%
    # month from data.table::
    .[month(obsDate) == use.month, last(obsDate)] %>%
    year
  
  last_full_fy_in_series <- yr2fy(last_full_yr_in_series)
  max_to_yr <- max_fy2yr(to_fy)
  
  if (!allow.projection && max_to_yr > last_full_yr_in_series) {
    stop("Not all elements of to_fy are in labour force data.")
  }
  
  # Use forecast::forecast to inflate forward
  forecast.series <- match.arg(forecast.series)
  
  if (AND(allow.projection,
          AND(forecast.series != "custom",
              max_to_yr > last_full_yr_in_series))) {
    # Labour force is monthly
    to_date <- fy2date(yr2fy(max_to_yr))
    months.ahead <- 
      12L * (year(to_date) - year(last.date.in.series)) +
      month(to_date) - month(last.date.in.series)
    
    forecasts <- 
      gforecast(lf.indices[["obsValue"]], 
                h = months.ahead, 
                level = forecast.level) %>%
      .subset2(forecast.series) %>%
      as.numeric 
    
    obsDate <- NULL
    lf.indices.new <- 
      data.table(obsValue = forecasts, 
                 obsDate  = seq.Date(last.date.in.series,
                                     to_date,
                                     by = "month")[-1]) %>%
      .[, obsTime := sprintf("%s-%02d", year(obsDate), month(obsDate))]

    lf.indices <- rbindlist(list(lf.indices, 
                                 lf.indices.new), 
                            use.names = TRUE, 
                            fill = TRUE)
  }
  
  lf.indices <- 
    lf.indices %>%
    .[month(obsDate) == use.month] %>%
    .[, "fy_year" := date2fy(obsDate)]

  
  if (AND(allow.projection,
          AND(max_to_yr > last_full_yr_in_series,
              forecast.series == "custom"))) {
    last_full_fy_in_series <- last(.subset2(lf.indices, "fy_year"))
    lf.indices <- append_custom_series(orig = lf.indices,
                                       custom.series = lf.series,
                                       max_to_yr = max_to_yr,
                                       last_full_yr_in_orig = last_full_yr_in_series,
                                       last_full_fy_in_orig = last_full_fy_in_series,
                                       cs = "lf.series")
  }
  
  inflator(labour_force, 
           from = from_fy, 
           to = to_fy, 
           inflator_table = lf.indices, 
           index.col = "obsValue",
           time.col = "fy_year",
           max.length = max.length)
}

#' @rdname lf_inflator
#' @examples
#' \dontrun{
#' lf_inflator(labour_force = 1, from_date = "2013-06-30", to_date = "2014-06-30")
#' }
lf_inflator <- function(labour_force = 1,
                        from_date = "2013-06-30",
                        to_date,
                        useABSConnection = FALSE) {
  obsTimeDate <- obsTime <- NULL
  # lf original
  if (useABSConnection) {
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

