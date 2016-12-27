#' Australia's population
#' 
#' @param date_quarter A character string (YYYY-QQ).
#' @param allow.projections If the date is beyond the ABS's confirmed data, should a projection be used?
#' @param fertility What fertility assumption should be used? Must be one of high, medium, low.
#' @param mortality The assumption of future life expectancy. Must be \code{high.LifeExpectancy} or \code{medium.LifeExpectancy}.
#' @return The population at \code{date_quarter}, or at the year if a projections.
#' @export 

aus_pop_qtr <- function(date_quarter,
                        allow.projections = TRUE,
                        fertility = c("high", "medium", "low"),
                        mortality = c("high.LifeExpectancy", 
                                      "medium.LifeExpectancy")){
  # CRAN Note avoidance
  obsTime <- NULL
  
  pop_data <- 
    "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.TT.Q/ABS?startTime=1981" %>%
    rsdmx::readSDMX(.) %>%
    as.data.frame %>%
    as.data.table %>%
    .[, .(obsTime, obsValue)]
  
  
  
  max_qtr <- 
    max(pop_data[["obsTime"]])
  
  if (max(date_quarter) > max_qtr){
    if (allow.projections){
      fertility <- match.arg(fertility)
      mortality <- match.arg(mortality)
      if (fertility == "high")
        fertility_selector <- 1
      if (fertility == "medium")
        fertility_selector <- 2
      if (fertility == "low")
        fertility_selector <- 3
      
      if (mortality == "high.LifeExpectancy")
        mortality_selector <- 1
      else 
        mortality_selector <- 2
      
      projector_url <- 
        paste0("http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/POP_PROJ_2011/",
               "0.3.TT.",
               fertility_selector, ".",
               mortality_selector, ".",
               "1.A/ABS?startTime=2012")
      
      projections <- 
        as.data.frame(rsdmx::readSDMX(projector_url)) %>%
        as.data.table
      
      date_year <- as.numeric(gsub("^.*([0-9]{4}).*$", "\\1", date_quarter))
      
      return(projections[obsTime %in% date_year][["obsValue"]])
    } else {
      dates <- pmin(max_qtr, date_quarter)
      warning("Using an earlier date than specified, viz. ", max_qtr)
      return(pop_data[obsTime %in% dates][["obsValue"]])
    }
  } else {
    return(pop_data[obsTime %in% date_quarter][["obsValue"]])
  }
}

