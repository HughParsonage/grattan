#' Append a custom series when you want a fixed forecast
#' @param orig The original series (with \code{fy_year})
#' @param custom.series The user-provided way of appending series.
#' @param max_to_fy The maximum user-provided \code{to_fy}.
#' @param last_full_yr_in_orig Last full year in \code{orig}.
#' @param last_full_fy_in_orig Last full financial year in \code{orig}.
#' @param cs The deparsed version of \code{orig} (a string, like 'lf.series').
#' @noRd 

append_custom_series <- function(orig,
                                 custom.series,
                                 max_to_yr,
                                 last_full_yr_in_orig,
                                 last_full_fy_in_orig,
                                 cs = deparse(substitute(custom.series))) {
  reqd_fys <-
    yr2fy(seq.int(from = min(last_full_yr_in_orig + 1L, 
                             fy2yr(last_full_fy_in_orig) + 1L),
                  to = max_to_yr)) %>%
    .[. %notin% .subset2(orig, "fy_year")]
  
  custom.series <-
    standardize_custom_series(custom.series,
                              cs = cs,
                              req.fys = reqd_fys)
  
  stopifnot(is.data.table(custom.series))
  
  if (max(.subset2(custom.series, "r")) > 1) {
    message("Some r > 1 in `", cs, "`. ",
            "This is unlikely rate of growth ", 
            "(r = 0.025 corresponds to 2.5% growth).")
  }
  
  # Need to connect the series, so if `last_full_yr_in_orig = 2018-19`,
  # then if `first_fy_in_custom_series`
  #   - leaves a gap (i.e. '2020-21') ==> error
  #   - is the following (i.e. '2019-20') ==> join nicely
  #   - equals or precedes '2018-19'  ==> custom series takes precedence
  input_series_fys <- .subset2(custom.series, "fy_year")
  first_fy_in_custom_series <- input_series_fys[[1L]]
 
  # nocov start
  if (first_fy_in_custom_series > next_fy(last_full_fy_in_orig)) {
    stop("Internal error: `first_fy_in_custom_series > next_fy(last_full_fy_in_orig)`\n\t", 
         "first_fy_in_custom_series = ", first_fy_in_custom_series, "\n\t",
         "last_full_fy_in_orig = ", last_full_fy_in_orig, "\n\t")
  }
  # nocov end
  
  # Is the following
  if (first_fy_in_custom_series == next_fy(last_full_fy_in_orig)) {
    last_obsValue_in_actual_series <- last(.subset2(orig, "obsValue"))
    
    obsValue <- r <- NULL
    custom.series[, obsValue := last_obsValue_in_actual_series * cumprod(1 + r)]
    
    out <- 
      rbindlist(list(orig, 
                     custom.series), 
                use.names = TRUE, 
                fill = TRUE) %>%
      # Ensure the date falls appropriately
      unique(by = "fy_year", fromLast = TRUE)
  } else {
    series_before_custom <- orig[fy_year < first_fy_in_custom_series]

    last_obsValue_in_actual_series <- last(series_before_custom[["obsValue"]])
    custom.series[, obsValue := last_obsValue_in_actual_series * cumprod(1 + r)]
    
    out <- 
      rbindlist(list(orig, 
                     custom.series), 
                use.names = TRUE, 
                fill = TRUE) %>%
      # Ensure the date falls appropriately
      unique(by = "fy_year", fromLast = TRUE)
  }
  
  out
  
}





#' Standardize custom series
#' @param custom.series A custom.series (possibly a single number, a list or a data.table)
#' @param cs A string (the deparse(substitute()) version of custom.series)
#' @param req.fys The required fys for the data.table (only required for atomic custom.series).
#' @noRd
standardize_custom_series <- function(custom.series, cs, req.fys) {
  
  if (is.null(custom.series)) {
    stopn("`", cs, " = NULL`, yet `forecast.series = ", '"custom"`. ', 
          "When `forecast.series = ", '"custom"`, ',
          "`", cs, "` must be single number or a list with names 'fy_year' and 'r'.",
          n = -3)
  }
  if (NEITHER(is.atomic(custom.series),
              is.list(custom.series))) {
    stopn("`", cs, "` had class ", class(custom.series)[1],
          ", but must either a single number ", 
          "or a list with names 'fy_year' and 'r'. ",
          n = -3)
  }
  
  if (is.atomic(custom.series)) {
    if (!is.numeric(custom.series)) {
      stopn("`", cs, "` was type ", typeof(custom.series), ". ", 
            "If using `", cs,
            "` as an atomic vector, ensure it is a single numeric vector.",
            n = -3)
    }
    
    if (length(custom.series) != 1L) {
      stopn("`", cs, "` had length ", length(custom.series), ". ", 
            "If using `", cs,
            "` as an atomic vector, ensure it is a single numeric vector.",
            n = -3)
    }
    
    
    
    return(data.table(fy_year = req.fys,
                      r = custom.series))
  }
  
  
  # Must be a list from here:
  
  if (is.null(names(custom.series))) {
    stopn("`", cs, "` is a list with no names. ", 
         "If using `", cs, "` as a list, ensure ",
         "its names are 'fy_year' and 'r'.",
         n = -3)
  }
  if (length(names(custom.series)) < 2L) {
    stopn("`", cs, "` had fewer than 2 names. ", 
         "If using `", cs, "` as a list, ensure ",
         "its names are 'fy_year' and 'r'.",
         n = -3)
  }
  if (names(custom.series)[1] != "fy_year") {
    stopn("`", cs, "` had first name ", '"', names(custom.series)[1], '". ',
         "If using `", cs, "` as a list, ensure ",
         "its names are 'fy_year' and 'r'.",
         n = -3)
  } 
  if (names(custom.series)[2] != "r") {
    stopn("`", cs, "` had second name ", '"', names(custom.series)[2], '". ',
         "If using `", cs, "` as a list, ensure ",
         "its names are 'fy_year' and 'r'.",
         n = -3)
  }
  if (!is.data.table(custom.series)) {
    if (length(custom.series[["r"]]) != length(custom.series[["fy_year"]])) {
      if (length(custom.series[["r"]]) != 1L) {
        stopn("`", cs, "` was a list with mismatching lengths: ", 
             vapply(custom.series, length, 1L), ". ", 
             "Ensure column `r` has length 1 ",
             "or ", length(custom.series[["fy_year"]]), 
             " (the length of column `fy_year`).",
             n = -3)
      }
    }
    custom.series <- as.data.table(custom.series)
  }
  
  input_series_fys <- .subset2(custom.series, "fy_year")
  
  if (any_notin(req.fys, input_series_fys)) {
    stopn("`", cs, "$fy_year` did not have the required financial years.\n\n", 
          "`", cs, "$fy_year` was\n\t",
          deparse(input_series_fys), "\n",
          "but needs to include\n\t",
          deparse(req.fys), ".",
          n = -3)
  }
  
  # Need to make sure that they're in the same order
  if (!identical(req.fys[req.fys %in% input_series_fys],
                 input_series_fys[input_series_fys %in% req.fys])) {
      stopn("`", cs, "$fy_year` had the required financial years but not in the correct order.\n\n", 
            "`", cs, "$fy_year` was\n\t",
            deparse(input_series_fys), "\n",
            "but needs to include and be in the same order as\n\t",
            deparse(req.fys), ".",
            n = -3)
    
    
  }
  
  return(custom.series)
}







