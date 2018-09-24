#' Generic inflator
#' @description Used to inflate variables in the sample file when there is no clear existing index.
#' Note this is an unexported function: it is not available to the end-user. 
#' @param vars A character vector of those variables within \code{.sample_file} for which forecasts are desired. 
#' @param h An integer, how many years ahead should the inflator be targeted.
#' @param fy.year.of.sample.file A string representing the financial year of \code{.sample_file}.
#' @param nonzero Should the forecast be taken on all values, or just nonzero values?
#' @param estimator What forecast element should be used: the point estimate (\code{"mean"}), or the \code{upper} or \code{lower} endpoint of a prediction interval?
#' @param pred_interval If \code{estimator} is \code{upper} or \code{lower}, what prediction interval are these the end points of?
#' @return A data table of two columns: \code{variable} containing \code{vars} and \code{inflator} equal to the inflator to be applied to that variable to inflate it ahead \code{h} years.

## For each variable we want an arima/ets model 
## and to use that to forecast ahead.
generic_inflator <- function(vars,
                             h,
                             fy.year.of.sample.file = "2012-13",
                             nonzero = FALSE,
                             estimator = "mean",
                             pred_interval = 80) {
  stopifnot(length(h) == 1L)
  stopifnot(is.integer(h), 
            h >= 0, 
            length(fy.year.of.sample.file) == 1L,
            all_fy(fy.year.of.sample.file))
  if (h == 0L) {
    return(data.table(variable = vars, 
                      inflator = 1))
  }
  vars_only_in_201314 <- c("MCS_Emplr_Contr", 
                           "MCS_Prsnl_Contr",
                           "MCS_Othr_Contr",
                           "MCS_Ttl_Acnt_Bal")
  
  switch(fy.year.of.sample.file, 
         "2012-13" = if (any(vars_only_in_201314 %chin% vars)) {
           stop("You have requested a projection / inflator of '",
                first(vars[vars %in% vars_only_in_201314]),
                "', yet `fy.year.ofy.sample.file = '2012-13'` but this variable ",
                "was not present till the 2013-14 sample file.")
         })
  
  
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
  
  forecaster <- function(x) {
    # Consider using hybridf
    
    # Condition for ets / auto.arima
    if (anyNA(x)) {
      forecast::auto.arima(stats::ts(x))
    } else {
      forecast::ets(x)
    }
  }
  
  forecast_and_extract <- function(fy.year, value) {
    max_year <- max_fy2yr(fy.year)
    list(fy_year = c(fy.year, yr2fy(max_year + seq_len(h))),
         value = c(value, extract_estimator(forecast_ahead_h(forecaster(value)))))
  }
  
  # from melt.data.table
  value <- NULL
  
  point_forecasts_by_var <- 
    mean_of_each_var %>%
    setDT %>%
    melt.data.table(id.vars = c("fy.year"), variable.factor = FALSE) %>%
    .[, forecast_and_extract(fy.year, value), by = "variable"] %>%
    .[complete.cases(.)] %>%
    setorderv("fy_year") %>%
    .[fy_year >= fy.year.of.sample.file] %>%
    .[, .(inflator = last_over_first(value)), by = "variable"]
}


