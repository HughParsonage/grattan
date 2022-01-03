#' CPI inflator
#' 
#' @name cpi_inflator
#' @export 
#' @param from_nominal_price (numeric) the price (or vector of prices) to be inflated
#' @param from_fy,to_fy (character) a character vector with each element in the form "2012-13" representing the financial years between which the CPI inflator is desired.
#' 
#' If both \code{from_fy} and \code{to_fy} are \code{NULL} (the default), \code{from_fy} is set to the previous financial year and \code{to_fy} to the current financial year, with a warning. Setting only one is an error.
#' @param adjustment What CPI index to use ("none" = raw series, "seasonal", or "trimmed" [mean]).
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? If \code{FALSE} (the default), a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date. 
#' 
#' If the SDMX connection fails, a message is emitted (not a warning) and
#' the function contines as if \code{useABSConnection = FALSE}.
#' 
#' The internal data was updated on 2022-01-03 to 2021-Q3. 
#' If using \code{useABSConnection = TRUE}, ensure you have \code{rsdmx (>= 0.5-10)} up-to-date.
#' @param allow.projection Should projections beyond the ABS's data be allowed?
#' @param accelerate.above An integer setting the threshold for 'acceleration'. 
#' When the maximum length of the arguments exceeds this value, calculate each unique value individually 
#' then combine. Set to 100,000 as a rule of thumb beyond which calculation speeds benefit
#' dramatically. Can be set to \code{Inf} to disable acceleration.
#' @examples 
#' cpi_inflator(100, from_fy = "2005-06", to_fy = "2014-15")
#' @return The value of \code{from_nominal_price} in real (\code{to_fy}) dollars.

cpi_inflator <- function(from_nominal_price = 1,
                         from_fy = NULL,
                         to_fy = NULL, 
                         adjustment = c("seasonal", "none", "trimmed.mean"),
                         useABSConnection = FALSE,
                         allow.projection = TRUE,
                         accelerate.above = 1e5L) {
  # CRAN
  obsTime <- obsValue <- NULL
  
  if (is.null(from_fy) && is.null(to_fy)) {
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
  
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(from_nominal_price, from_fy, to_fy)
  
  if (max.length == 0L) {
    warning("Zero-length arguments provided, returning double(0).")
    return(double(0))
  }
  
  adjustment <- match.arg(adjustment, several.ok = FALSE)

  
  
  if (max.length > accelerate.above && 
      # don't connect for every group
      !useABSConnection &&
      length(from_nominal_price) == 1L) {
    if (length(to_fy) == 1L) {
      cpi_fun <- function(x) {
        cpi_inflator(from_nominal_price = from_nominal_price[[1L]],
                     from_fy = x,
                     to_fy = to_fy[[1L]],
                     adjustment = adjustment[[1L]], 
                     useABSConnection = FALSE,
                     allow.projection = allow.projection[[1L]],
                     accelerate.above = Inf)
      }
      return(accel_repetitive_input(from_fy, cpi_fun))
    }
    
    if (length(from_fy) == 1L) {
      cpi_fun <- function(x) {
        cpi_inflator(from_nominal_price = from_nominal_price[[1L]],
                     from_fy = from_fy[[1L]], 
                     to_fy = x,
                     adjustment = adjustment[[1L]], 
                     useABSConnection = FALSE,
                     allow.projection = allow.projection[[1L]],
                     accelerate.above = Inf)
      }
      return(accel_repetitive_input(to_fy, cpi_fun))
    }
  }
  
  
  cpi.indices <- 
    if (useABSConnection) {
      if (!requireNamespace("rsdmx", quietly = TRUE)) {
        stop("`useABSConnection = TRUE`, yet package:rsdmx is not installed.")  # nocov
      }
      url <-
        switch(adjustment, 
               "none" = "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10.Q/ABS?startTime=1948", 
               "seasonal" = "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948",
               "trimmed.mean" = "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948")
      
      # nocov start
      sdmx_ok <- TRUE
      cpi <- tryCatch(rsdmx::readSDMX(url),
                      error = function(e) {
                        sdmx_ok <<- FALSE
                        message("SDMX failed with error ", e$m,
                                "falling back to useABSConnection = FALSE.")
                        switch(adjustment, 
                               "none" = cpi_unadj_fy,
                               "seasonal" = cpi_seasonal_adjustment_fy,
                               "trimmed.mean" = cpi_trimmed_fy)
                      })
      # nocov end
      if (sdmx_ok) {
        message("Using ABS sdmx connection")
        as.data.frame(cpi) %>%
          as.data.table %>%
          .[endsWith(obsTime, "Q1")] %>%
          .[, "fy_year" := yr2fy(as.integer(sub("-Q1", "", obsTime, fixed = TRUE)))]
      } else {
        cpi  # nocov 
      }
    } else {
      switch(adjustment, 
             "none" = cpi_unadj_fy,
             "seasonal" = cpi_seasonal_adjustment_fy,
             "trimmed.mean" = cpi_trimmed_fy)
    }
  
  
  
  permitted_fys <- .subset2(cpi.indices, "fy_year")
  earliest_from_fy <- permitted_fys[[1L]]
  cpi_table_nom <-
    switch(adjustment, 
           "none" = "first instance of the unadjusted CPI", 
           "seasonal" = "first instance of the seasonally adjusted CPI",
           "trimmed.mean" = "first instance of the trimmed mean CPI")
  
  the_min.yr <-
    switch(adjustment, 
           "none" = min.cpi_unadj.yr,
           "seasonal" = min.cpi_seasonal_adjustment.yr,
           "trimmed.mean" = min.cpi_trimmed.yr)
  the_max.yr <-
    switch(adjustment, 
           "none" = max.cpi_unadj.yr,
           "seasonal" = max.cpi_seasonal_adjustment.yr,
           "trimmed.mean" = max.cpi_trimmed.yr)
  
  from_fy <- validate_fys_permitted(from_fy,
                                    min.yr = the_min.yr,
                                    # else 2050L because we will need max year later
                                    max.yr = if (!allow.projection) the_max.yr else 2050L,
                                    deparsed = "from_fy", 
                                    allow.projection = allow.projection,
                                    earliest_permitted_financial_year = cpi_table_nom)
  to_fy <- validate_fys_permitted(to_fy,
                                  min.yr = the_min.yr,
                                  max.yr = if (!allow.projection) the_max.yr else 2050L,
                                  deparsed = "to_fy", 
                                  allow.projection = allow.projection,
                                  earliest_permitted_financial_year = cpi_table_nom)
  
  if (max_fy2yr(to_fy) > the_max.yr || 
      max_fy2yr(from_fy) > the_max.yr) {
    # Number of years beyond the data our forecast must reach
    years.beyond <- max_fy2yr(to_fy) - max_fy2yr(permitted_fys)
    cpi_index_forecast <-
      cpi.indices %$%
      gforecast(obsValue, h = years.beyond) %$%
      as.numeric(mean)
    
    cpi.indices.new <- 
      setDT(list(fy_year = yr2fy(seq(max_fy2yr(permitted_fys) + 1L,
                                     max_fy2yr(to_fy),
                                     by = 1L)),
                 obsValue = cpi_index_forecast))
    
    # TODO: fy should inherit 'character'
    cpi.indices.new[, fy_year := as.character(fy_year)]  
    
    cpi.indices <-
      rbindlist(list(cpi.indices, cpi.indices.new),
                use.names = TRUE,
                fill = TRUE)
  }
  
  inflator(from_nominal_price,
           from = from_fy,
           to = to_fy,
           inflator_table = cpi.indices,
           index.col = "obsValue", 
           time.col = "fy_year",
           max.length = max.length)
  
}


