#' Generic inflator
#' @description Used to inflate variables in the sample file when there is no clear existing index.
#' @param vars A character vector of those variables within \code{.sample_file} for which forecasts are desired. 
#' @param h An integer, how many years ahead should the inflator be targeted.
#' @param fy.year.of.sample.file A string representing the financial year of \code{.sample_file}.
#' @param nonzero Should the forecast be taken on all values, or just nonzero values?
#' @param estimator What forecast element should be used: the point estimate (\code{"mean"}), or the \code{upper} or \code{lower} endpoint of a prediction interval?
#' @param pred_interval If \code{estimator} is \code{upper} or \code{lower}, what prediction interval are these the end points of?
#' @return A data table of two columns: \code{variable} containing \code{vars} and \code{inflator} equal to the inflator to be applied to that variable to inflate it ahead \code{h} years.

## For each variable we want an arima/ets model 
## and to use that to forecast ahead.
generic_inflator <- function(vars, h, fy.year.of.sample.file = "2012-13", nonzero = FALSE, estimator = "mean", pred_interval = 80){
  stopifnot(length(h) == 1L)
  if (h == 0L){
    return(data.table(variable = vars, 
                      inflator = 1))
  }
  
  stopifnot(is.integer(h), 
            h >= 0, 
            is.fy(fy.year.of.sample.file))
  
  
  
  forecast_ahead_h <- function(object, ...){
    forecast::forecast(object, h = h, level = pred_interval, ...)
  }
  
  extract_estimator <- function(.prev){
    switch(estimator, 
           "mean"  = {
             out <- magrittr::use_series(magrittr::extract(.prev, "mean"), "mean")
           },
           "upper" = {
             out <- magrittr::use_series(magrittr::extract(.prev, "upper"), "upper")
           },
           "lower" = {
             out <- magrittr::use_series(magrittr::extract(.prev, "lower"), "lower")
           })
    as.numeric(out)
  }
  
  if (!nonzero){
    mean_of_each_var <- mean_of_each_taxstats_var[, .SD, .SDcols = c("fy.year", vars)]
  } else {
    # Forecast only on the mean of nonzero values
    mean_of_each_var <- meanPositive_of_each_taxstats_var[, .SD, .SDcols = c("fy.year", vars)]
  }
  
  forecaster <- function(x){
    # Consider using hybridf
    
    # Condition for ets / auto.arima
    if (!anyNA(x)){
      forecast::ets(x)
    } else {
      forecast::auto.arima(stats::ts(x))
    }
  }
  
  point_forecasts_by_var <- 
    mean_of_each_var %>%
    as.data.table(.) %>%
    melt.data.table(id.vars = c("fy.year")) %>% 
    base::split(.$variable) %>%
    purrr::map(~forecaster(.$value)) %>%
    purrr::map(forecast_ahead_h) %>%
    purrr::map(extract_estimator) 
  
  # CRAN note avoidance
  fy_year <- NULL
  rbindlist(list(as.data.table(mean_of_each_var), 
                 as.data.table(point_forecasts_by_var)), 
            use.names = TRUE, 
            fill = TRUE) %>%
    .[, fy_year := yr2fy(1:.N - 1 + fy2yr(first(fy.year)))] %>% 
    # last(fy_year) is the fy_year corresponding to h, the target. 
    .[fy_year %in% c(fy.year.of.sample.file, last(fy_year))] %>% 
    .[, lapply(.SD, last_over_first), .SDcols = vars] %>%
    melt.data.table(measure.vars = names(.), variable.name = "variable", value.name = "inflator")
}


