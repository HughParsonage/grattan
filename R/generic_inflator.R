#' Generic inflator
#' @description Used to inflate variables in the sample file when there is no clear existing index.
#' @param vars A character vector of those variables within \code{.sample_file} for which forecasts are desired. 
#' @param h An integer, how many years ahead should the inflator be targeted.
#' @param fy.year.of.sample.file A string representing the financial year of \code{.sample_file}.
#' @param nonzero Should the forecast be taken on all values, or just nonzero values?
#' @param estimator What forecast element should be used: the point estimate (\code{"mean"}), or the \code{upper} or \code{lower} endpoint of a prediction interval?
#' @param pred_interval If \code{estimator} is \code{upper} or \code{lower}, what prediction interval are these the end points of?
#' @return A data table of two columns: \code{variable} containing \code{vars} and \code{inflator} equal to the inflator to be applied to that variable to inflate it ahead \code{h} years.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %$%
#' @importFrom data.table as.data.table

## For each variable we want an arima/ets model 
## and to use that to forecast ahead.
generic_inflator <- function(vars, h, fy.year.of.sample.file = "2012-13", nonzero = FALSE, estimator = "mean", pred_interval = 80){
  if (h == 0L){
    return(data.table::data.table(variable = vars, 
                                  inflator = 1))
  }
  
  stopifnot(is.integer(h), 
            h >= 0, 
            is.fy(fy.year.of.sample.file))
  
  
  
  forecast_ahead_h <- function(object, ...){
    forecast::forecast(object, h = h, level = pred_interval, ...)
  }
  
  extract_estimator <- function(.prev){
    if (estimator == "mean")
      as.numeric(magrittr::use_series(magrittr::extract(.prev, "mean"), "mean"))
    
    if (estimator == "upper")
      magrittr::use_series(magrittr::extract(.prev, "upper"), "upper")
    
    if (estimator == "lower")
      magrittr::use_series(magrittr::extract(.prev, "lower"), "lower")
  }
  
  if (estimator == "mean"){
    extract_estimator <- function(.prev){
      as.numeric(magrittr::use_series(magrittr::extract(.prev, "mean"), "mean"))
    }
  } else {
    if (estimator == "upper"){
      extract_estimator <- function(.prev){
        as.numeric(magrittr::use_series(magrittr::extract(.prev, "upper"), "upper"))
      }
    } else {
      if (estimator == "lower"){
        extract_estimator <- function(.prev){
          as.numeric(magrittr::use_series(magrittr::extract(.prev, "lower"), "lower"))
        }
      }
    }
  }
  
  if (!nonzero){
    mean_of_each_var <- 
      taxstats::sample_files_all %>%
      dplyr::select_(.dots = c("fy.year", vars)) %>%
      select_which_(is.numeric, "fy.year") %>%
      dplyr::group_by_("fy.year") %>%  
      dplyr::summarise_each(dplyr::funs(MeanNumeric)) 
} else {
    # Forecast only on the mean of nonzero values
    mean_of_each_var <- 
      taxstats::sample_files_all %>%
      dplyr::select_(.dots = c("fy.year", vars)) %>%
      select_which_(is.numeric, "fy.year") %>%
      dplyr::group_by_("fy.year") %>%  
      dplyr::summarise_each_(dplyr::funs(mean_of_nonzero))
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
    data.table::as.data.table(.) %>%
    data.table::melt.data.table(id.vars = c("fy.year")) %>% 
    base::split(.$variable) %>%
    purrr::map(~forecaster(.$value)) %>%
    purrr::map(forecast_ahead_h) %>%
    purrr::map(extract_estimator) 
  
  # CRAN avoidance
  fy_year <- NULL
  data.table::rbindlist(list(data.table::as.data.table(mean_of_each_var), 
                             data.table::as.data.table(point_forecasts_by_var)), 
                        use.names = TRUE, 
                        fill = TRUE) %>%
    .[ ,fy_year := yr2fy(1:.N - 1 + fy2yr(dplyr::first(fy.year)))] %>% 
    dplyr::filter(fy_year %in% c(fy.year.of.sample.file, dplyr::last(fy_year))) %>% 
    dplyr::summarise_each(dplyr::funs(last_over_first), -c(fy_year, fy.year)) %>%
    data.table::as.data.table(.) %>%
    data.table::melt.data.table(., measure.vars = names(.), variable.name = "variable", value.name = "inflator")
  
  ## We use inflators that we know to be useful. (Sw_amt is a wage inflator)
  ## Otherwise we use 
  ## For those columns with nonnegative values, we use the inflator by var mean 0
  ## For the remainder, inflator by var.
  
  ## The inflators look too large.

}


