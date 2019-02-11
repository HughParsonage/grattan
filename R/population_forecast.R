
population_forecast <- function(to_year = NULL,
                                YOBs = NULL) {
  if (is.null(to_year)) {
    to_year <- year(Sys.Date()) + 1L
    message("`to_year` is missing, so using `to_year = ", to_year, "`.")
  }
  
  DT <- copy(aus_pop_qtr_age(tbl = TRUE))
  DT[, YOB := year(Date) - Age]
  
  # Quarters start at 1 in calendar year
  last_month <- month(DT[, last(Date)])
  last_qtr <- get_qtr(DT[, last(Date)])
  first_qtr <- get_qtr(DT[, first(Date)])
  
  h <- 0L
  forecast_yob1 <- function(v, dates) {
    .ts <- 
      ts(v,
         frequency = 4,
         start = c(min(year(dates)), 
                   first_qtr))
    
    h <<- 4L * (to_year - max(year(dates))) - (4L - last_qtr)
    k <<- h
    the_forecast <<- forecast::forecast(v, h = h, level = 95)
    
    
    list(f_m = as.double(the_forecast[["mean"]]),
         Dates = seq.Date(from = max(dates),
                          by = "3 months",
                          length.out = h + 1L)[-1L])
  }
  out <- DT
  if (!is.null(YOBs)) {
    out <- out[YOB %in% YOBs]
  } 
  if (length(YOBs) != 1L) { # not > 1 since NULL means 'all'
    out[, forecast_yob1(Value, Date), keyby = "YOB"]
  } else {
    out[, forecast_yob1(Value, Date)]
  }
}


