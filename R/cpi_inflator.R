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
#' The internal data was updated on 2018-08-10 to 2018-Q2. 
#' If using \code{useABSConnection = TRUE}, ensure you have \code{rsdmx (>= 0.5-10)} up-to-date.
#' @param allow.projection Should projections beyond the ABS's data be allowed?
#' @examples 
#' cpi_inflator(100, from_fy = "2005-06", to_fy = "2014-15")
#' @return The value of \code{from_nominal_price} in real (\code{to_fy}) dollars.

cpi_inflator <- function(from_nominal_price = 1,
                         from_fy = NULL,
                         to_fy = NULL, 
                         adjustment = c("seasonal", "none", "trimmed.mean"),
                         useABSConnection = FALSE,
                         allow.projection = TRUE) {
  # CRAN
  obsTime <- obsValue <- to_index <- from_index <- NULL
  
  if (is.null(from_fy) && is.null(to_fy)){
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
  
  
  
  
  if (max.length > 1e5L && 
      # don't connect for every group
      !useABSConnection &&
      length(from_nominal_price) == 1L) {
    if (length(to_fy) == 1L) {
      return(accel_repetitive_input(from_fy,
                                    cpi_inflator,
                                    from_nominal_price = from_nominal_price[[1L]],
                                    to_fy = to_fy[[1L]],
                                    adjustment = adjustment[[1L]], 
                                    useABSConnection = FALSE, 
                                    allow.projection = allow.projection[[1L]]))
    } else {
      cpi_fun <- function(x) {
        cpi_inflator(from_nominal_price = from_nominal_price[[1L]],
                     from_fy = from_fy[[1L]], 
                     to_fy = x,
                     adjustment = adjustment[[1L]], 
                     useABSConnection = FALSE,
                     allow.projection = allow.projection[[1L]])
      }
      return(accel_repetitive_input(to_fy, cpi_fun))
    }
  }
  
  
  
  if (useABSConnection) {
    switch(adjustment, 
           "none" = url <-     
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.10001.10.Q/ABS?startTime=1948", 
           
           "seasonal" = url <- 
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999901.10+20.Q/ABS?startTime=1948",
           
           "trimmed.mean" = url <- 
             "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/CPI/1.50.999902.10+20.Q/ABS?startTime=1948")
    
    cpi <- rsdmx::readSDMX(url)
    message("Using ABS sdmx connection")
    cpi <- as.data.frame(cpi)
  } else {
    switch(adjustment, 
           "none" = cpi <- cpi_unadj,
           "seasonal" = cpi <- cpi_seasonal_adjustment,
           "trimmed.mean" = cpi <- cpi_trimmed)
  }
  
  cpi.indices <- 
    as.data.table(cpi) %>%
    .[endsWith(obsTime, "Q1")] %>%
    .[, fy_year := yr2fy(as.integer(sub("-Q1", "", obsTime, fixed = TRUE)))]
  
  permitted_fys <- .subset2(cpi.indices, "fy_year")
  earliest_from_fy <- permitted_fys[[1L]]
  cpi_table_nom <-
    switch(adjustment, 
           "none" = "unadjusted",
           "seasonal" = "seasonally adjusted",
           "trimmed.mean" = "trimmed mean")
  
  if (!all_fy(from_fy, permitted = permitted_fys)) {
    if (!is.character(from_fy)) {
      stop("`from_fy` was type ", '"', typeof(from_fy), '", ', 
           "but must be a character vector. ", 
           "\n\n",
           "Ensure all elements of `from_fy` are valid financial years satisfying `",
           '"', earliest_from_fy, '"',
           " <= from_fy <= ", '"',
           last(permitted_fys), '"', "`. ")
    }
    if (max.length == 1L) {
      if (!is.fy(from_fy)) {
        stop("`from_fy = ", '"', from_fy, '"', "` was not a valid financial year. ",
             "\n\n",
             "Ensure all elements of `from_fy` are valid financial years satisfying `",
             '"', earliest_from_fy, '"',
             " <= from_fy <= ", '"',
             last(permitted_fys), '"', "`. ")
      }
      
      if (from_fy < earliest_from_fy) {
        stop("`from_fy = ", '"', from_fy, '"', "` which is earlier than the first ", 
             "instance of the ", cpi_table_nom, " CPI, ", '"', earliest_from_fy, '".', 
             "\n\n",
             "Ensure all elements of `from_fy` are valid financial years satisfying `",
             '"', earliest_from_fy, '"',
             " <= from_fy <= ", '"',
             last(permitted_fys), '"', "`. ")
      }
    } else {
      
      first_early_fy <- first(from_fy[from_fy %notin% permitted_fys])
      if (!is.fy(first_early_fy)) {
        stop("`from_fy` contained ", '"', first_early_fy, '"', 
             " which is not a valid financial year. ", 
             "\n\n",
             "Ensure all elements of `from_fy` are valid financial years satisfying `",
             '"', earliest_from_fy, '"',
             " <= from_fy <= ", '"',
             last(permitted_fys), '"', "`. ")
      }
      stop("`from_fy` contained ", '"', first_early_fy, '"', 
           " which is earlier than the first ", 
           "instance of the ", cpi_table_nom, " CPI, ", '"', earliest_from_fy, '".',
           "\n\n",
           "Ensure all elements of `from_fy` are valid financial years satisfying `",
           '"', earliest_from_fy, '"',
           " <= from_fy <= ", '"',
           last(permitted_fys), '"', "`. ")
    }
  }
  
  
  if (!all_fy(to_fy, permitted_fys)) {
    if (!allow.projection) {
      if (length(to_fy) == 1L) {
        stop("`to_fy = ", '"', to_fy, '"', "` yet `allow.projection = FALSE`. ", 
             "The latest to_fy that may be used is ", max(cpi.indices$fy_year), ". ", 
             "Set `allow.projection = TRUE` or ensure `to_fy` is earlier than ", 
             max(cpi.indices$fy), ".")
      } else {
        first_late_fy <- first(to_fy[to_fy %notin% cpi.indices$fy_year])
        stop("`to_fy` contains ", '"', first_late_fy, '"', ", yet `allow.projection = FALSE`. ", 
             "The latest to_fy that may be used is ", max(cpi.indices$fy_year), ". ",
             "Set `allow.projection = TRUE` or ensure `to_fy` is earlier than ", 
             max(cpi.indices$fy), ".")
      }
    } else {
      if (!all_fy(to_fy)) {
        if (!all(is.fy(to_fy))) {
          if (length(to_fy) == 1L) {
            stop("`to_fy = ", '"', to_fy, '"`',
                 " which is not a valid financial year.",
                 "\n\n", 
                 "Ensure all elements of `to_fy` are valid financial years.")
          } else {
            first_to_fy <- to_fy[which.min(is.fy(to_fy))]
            stop("`to_fy` contained ",
                 '"', first_to_fy, '"',
                 " which is an invalid financial year.",
                 "\n\n", 
                 "Ensure all elements of `to_fy` are valid financial years.")
          }
        }
      }
      
      # Number of years beyond the data our forecast must reach
      years.beyond <- max(fy2yr(to_fy)) - max(fy2yr(permitted_fys))
      cpi_index_forecast <-
        cpi.indices %$%
        gforecast(obsValue, h = years.beyond) %$%
        as.numeric(mean)
      
      cpi.indices.new <- 
        setDT(list(fy_year = yr2fy(seq(max(fy2yr(permitted_fys)) + 1L,
                                       max(fy2yr(to_fy)),
                                       by = 1L)),
                   obsValue = cpi_index_forecast))
      cpi.indices <-
        rbindlist(list(cpi.indices, cpi.indices.new),
                  use.names = TRUE,
                  fill = TRUE)
    }
  }
  
  inflator(from_nominal_price,
           from = from_fy,
           to = to_fy,
           inflator_table = cpi.indices,
           index.col = "obsValue", 
           time.col = "fy_year")
  
}
