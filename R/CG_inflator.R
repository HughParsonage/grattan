#' Forecasting capital gains
#' 
#' @name CG_population_inflator
#' @param x To be inflated.
#' @param from_fy,to_fy Financial years designating the inflation period.
#' @param forecast.series One of \code{"mean"}, \code{"lower"}, \code{"upper"}. What estimator to use in forecasts. \code{"lower"} and \code{"upper"} give the lower and upper boundaries of the 95\% prediction interval.
#' @param cg.series (Not implemented.)
#' @return For \code{CG_population_inflator}, the number of individuals estimated to incur capital gains in \code{fy_year}. 
#' For \code{CG_inflator}, an estimate of the nominal value of (total) capital gains in \code{to_fy} relative to the nominal value in \code{from_fy}. 

CG_population_inflator <- function(x = 1, 
                                   from_fy, 
                                   to_fy, 
                                   forecast.series = "mean", 
                                   cg.series){
  stopifnot(all_fy(c(from_fy, to_fy)))
  stopifnot(forecast.series %in% c("mean", "lower", "upper", "custom"))
  
  last_fy <- max(from_fy, to_fy)
  last_year <- fy2yr(last_fy)
  
  input <- 
    data.table(x = x, from_fy = from_fy, to_fy = to_fy)
  
  nse_forecast_series <- forecast.series
  
  out_tbl <- 
    cg_inflators_1516 %>% 
    copy %>%
    .[forecast.series == nse_forecast_series]
  
  input %>%
    merge(out_tbl, by.x = "from_fy", by.y = "fy_year", all.x = TRUE, sort = FALSE) %>%
    setnames("n_CG", "n_CG_from") %>% 
    merge(out_tbl, by.x = "to_fy", by.y = "fy_year", all.x = TRUE, sort = FALSE) %>% 
    setnames("n_CG", "n_CG_to") %$%
    {
      x * n_CG_to / n_CG_from
    }
}
  
#' @rdname CG_population_inflator

CG_inflator <- function(x = 1, from_fy, to_fy, forecast.series = "mean"){
  prohibit_vector_recycling(x, from_fy, to_fy)
  stopifnot(is.numeric(x), all_fy(from_fy), all_fy(to_fy))
  
  nse_forecast_series <- forecast.series
  cg_inflators_tbl <- 
    cg_inflators_1516[forecast.series == nse_forecast_series]
  
  
  # Else NAs.
  stopifnot(all(to_fy %in% cg_inflators_1516[["fy_year"]]),
            all(from_fy %in% cg_inflators_1516[["fy_year"]]))

  # CRAN Note avoidance
  ordering <- NULL
  input <- 
    data.table(x = x, from_fy = from_fy, to_fy = to_fy) %>% 
    .[, ordering := 1:.N]

  
  
  raw_out <- 
    input %>%
    merge(cg_inflators_tbl, by.y = "fy_year", by.x = "from_fy", all.x = TRUE) %>%
    setnames("zero_discount_Net_CG_total", "from_cg") %>% 
    merge(cg_inflators_tbl, by.y = "fy_year", by.x = "to_fy", all.x = TRUE) %>%
    setnames("zero_discount_Net_CG_total", "to_cg") %>%
    setorderv("ordering") %$%
    {
      x * to_cg / from_cg
    }
  
  # The tax expenditures reflect totals, not means, so we need to adjust for
  # totals.
  raw_out / CG_population_inflator(1, from_fy = from_fy, to_fy = to_fy)
}
