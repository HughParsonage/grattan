
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
      stats::ts(if (do_log) log(v + 1) else v,
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
  
  if (length(YOBs) != 1L) { # not > 1 since NULL means 'all'
    if (requireNamespace("future.apply", quietly = TRUE) &&
        requireNamespace("future", quietly = TRUE) &&
        # Don't bother with future.apply if we just
        # have sequential planning
        !inherits(future::plan(), "sequential")) {
      
      setkeyv(DT, "YOB")
      X <- first(.subset2(DT, "YOB")):last(.subset2(DT, "YOB"))
      out <- 
        future.apply::future_lapply(seq_along(X), 
                                    function(i) {
                                      DT[.(X[i]), 
                                         forecast_yob1(Value, Date)]
                                    }) %>%
        rbindlist(idcol = "YOBi") %>%
        .[, YOB := X[YOBi]] %>%
        .[, YOBi := NULL] %>%
        setattr("cols", "YOB") %>%
        .[]
    } else {
      out <- DT[, forecast_yob1(Value, Date), keyby = "YOB"]
    }
  } else {
    out <- DT[, forecast_yob1(Value, Date)]
    out[, YOB := YOBs]
    setkey(out, YOB)
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

population_forecast_age_range <- function(from_fy, to_fy, age_range = 0:11) {
  
  the_populations <- population_forecast(to_year = fy2yr(to_fy) + 1L, include_tbl = TRUE, do_log = TRUE)
  the_populations[, Age := year(Date) - YOB]
  the_populations <- the_populations[Age %between% c(15L, 90L)][month(Date) == 6L]
  the_populations[, age_range := age2age_range(Age)]
  pop_indices <- the_populations[, .(Population = sum(Population)), keyby = .(age_range, Date)]
  pop_indices[, fy := date2fy(Date)]
  pop_indices[, .(i = inflator(from = from_fy,
                       to = to_fy, 
                       index.col = "Population",
                       time.col = "fy", 
                       inflator_table = .SD)),
      keyby = "age_range"]
  
}

project_population <- function(DT,
                               wt_col,
                               age_col = NULL,
                               age_range_col = NULL,
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
  
  if (is.null(age_col) && is.null(age_range_col)) {
    stop("`age_col` and `age_range_col` were both NULL, but one must be provided.")
  }
  
  if (!is.null(age_col) && !is.null(age_range_col)) {
    stop("`age_col` and `age_range_col` were both provided.")
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







