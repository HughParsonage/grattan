
population_forecast <- function(to_year = NULL,
                                YOBs = NULL,
                                include_tbl = FALSE,
                                do_log = FALSE) {
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
      ts(if (do_log) log(v + 1) else v,
         frequency = 4,
         start = c(min(year(dates)), 
                   first_qtr))
    
    h <- 4L * (to_year - max(year(dates))) - (4L - last_qtr)
    the_forecast <- forecast::forecast(.ts, h = h, level = 95)
    
    the_y <- as.numeric(the_forecast[["mean"]])
    
    list(Date = seq.Date(from = max(dates),
                          by = "3 months",
                          length.out = h + 1L)[-1L],
         Population = as.integer(if (do_log) {
           exp(the_y) - 1 
         } else {
           the_y
         }))
  }
  if (!is.null(YOBs)) {
    DT <- DT[YOB %in% YOBs]
  } 
  out <- 
    if (length(YOBs) != 1L) { # not > 1 since NULL means 'all'
      DT[, forecast_yob1(Value, Date), keyby = "YOB"]
    } else {
      DT[, forecast_yob1(Value, Date)]
    }
  
  if (include_tbl) {
    rbindlist(list(DT[, .(YOB, Date, Population = Value, isForecast = FALSE)], 
                   out[, .(YOB, Date, Population, isForecast = TRUE)]),
              use.names = TRUE, 
              fill = TRUE) %>%
      .[]
  } else {
    out
  }
  
}

project_population <- function(DT,
                               wt_col,
                               age_col,
                               from = NULL,
                               to = NULL) {
  if (missing(wt_col)) {
    stop("`wt_col` was missing, with no default.")
  }
  if (length(wt_col) != 1L) {
    stop("`col` had length ", length(col), ".", 
         "Ensure `col` specifies a column in `DT` containing the weight to project.")
  }
  if (hasntName(DT, wt_col)) {
    stop("`DT` did not have the specified column: `wt_col = ", wt_col, ".")
  }
  
  if (missing(age_col)) {
    stop("`wt_col` was missing, with no default.")
  }
  if (length(age_col) != 1L) {
    stop("`col` had length ", length(col), ".", 
         "Ensure `col` specifies a column in `DT` containing the weight to project.")
  }
  if (hasntName(DT, age_col)) {
    stop("`DT` did not have the specified column: `wt_col = ", wt_col, ".")
  }
  if (!is.numeric(AgeCol_values <- .subset2(DT, age_col))) {
    stop("Age must be numeric.")
  }
  
  last_Pop <- 
    aus_pop_qtr_age(date = NULL, tbl = TRUE) %>% 
    .[, .(Value = last(Value)), keyby = "Age"]
  
  if (anyNA(match(x = 1:100, AgeCol_values))) {
    
  } else {
    Pop <- population_forecast(to_year = fy2yr(to), YOBs = NULL)
  }
  
  last_Pop[Pop, on = "Age"]
}




