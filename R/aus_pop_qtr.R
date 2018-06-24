#' Australia's population
#' 
#' @param date_quarter A character string (YYYY-QQ).
#' @param allow.projections If the date is beyond the ABS's confirmed data, should a projection be used?
#' @return The population at \code{date_quarter}, or at the most recent year in the data if projections are disallowed.
#' @export 

aus_pop_qtr <- function(date_quarter,
                        allow.projections = TRUE){
  # CRAN Note avoidance
  obsTime <- obsValue <- NULL
  if (any(!grepl("^[12][0-9]{3}-Q[1-4]$", date_quarter))) {
    bad_qtrs <- which(!grepl("^[12][0-9]{3}-Q[1-4]$", date_quarter))
    switch(min(length(bad_qtrs), 3),
           stop("Entry ", bad_qtrs, " was not in the correct form."),
           stop("Entries ", bad_qtrs[1], " and ", bad_qtrs[2], " were not in the correct form."),
           stop("Entry ", bad_qtrs[1], " was not in the right form. ", 
                "There were ", length(bad_qtrs) - 1,
                " other bad entries."))
  }
  
  pop_data <- aus_pop_by_yearqtr
  
  max_qtr <- max(pop_data[["obsTime"]])
  max_date_quarter <- max(date_quarter)
  
  if (max_date_quarter > max_qtr){
    if (allow.projections) {
      h <- qtrs_ahead(max_qtr, max_date_quarter)
      # nocov start
      if (substr(max_qtr, 7, 7) == "4") {
        next_qtr <- 1L 
      } else {
        next_qtr <- as.integer(substr(max_qtr, 7, 7)) + 1L
      }
      # nocov end
      
      next_year <- as.integer(substr(max_qtr, 0, 4))
      Quarter <- Year <- NULL
      
      new_pop_data <-
        data.table(obsValue = as.double(gforecast(pop_data[["obsValue"]], h = h)[["mean"]]),
                   Quarter = rep((next_qtr + 0:3) %% 4, length.out = h)) %>%
        .[Quarter == 0, Quarter := 4] %>%
        # If next_year = next_year + 1, cumsum(Quarter == 1) will add one immediately
        .[, Year := next_year + cumsum(Quarter == 1)] %>%
        .[, obsTime := paste0(Year, "-Q", Quarter)] %>%
        .[, .(obsTime, obsValue)] %>%
        rbind(pop_data, .)
      return(new_pop_data[obsTime %in% date_quarter][["obsValue"]])
    } else {
      dates <- pmin(max_qtr, date_quarter)
      warning("Using an earlier date than specified, viz. ", max_qtr)
      return(pop_data[obsTime %in% dates][["obsValue"]])
    }
  } else {
    return(pop_data[obsTime %in% date_quarter][["obsValue"]])
  }
}

