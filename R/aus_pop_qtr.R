#' Australia's population
#' 
#' @param date as character string(YYYY-QQ)
#' @param allow.projections If the date is beyond the ABS's confirmed data, should a projection be used?
#' @param fertility What fertility assumption should be used? (high, medium, low)
#' @param mortality The assumption of future life expectancy. Should be \code{high.LifeExpectancy} or \code{medium.LifeExpectancy}.
#' @return the population at \code{date}, or at the year if a projections.
#' 

aus_pop_qtr <- function(date_quarter, allow.projections = TRUE, fertility = "high", mortality = "high.LifeExpectancy"){
  pop_data <- 
    dplyr::select_(data.table::data.table(as.data.frame(rsdmx::readSDMX("http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.TT.Q/ABS?startTime=1981"))), 
                  .dots = c("obsTime", "obsValue"))
  
  max_qtr <- 
    max(pop_data$"obsTime")
  
  if (date_quarter > max_qtr){
    if (allow.projections){
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
      
      projector_url <- paste0("http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/POP_PROJ_2011/",
                              "0.3.TT.",
                              fertility_selector, ".",
                              mortality_selector, ".",
                              "1.A/ABS?startTime=2012")
      
      projections <- as.data.frame(rsdmx::readSDMX(projector_url))
      
      date_year <- as.numeric(gsub("^.*([0-9]{4}).*$", "\\1", date_quarter))
      
      return(projections[projections$obsTime == date_year, ]$"obsValue")
    } else {
    warning("Using an earlier date than specified, viz. ", max_qtr)
    return(pop_data[pop_data$obsTime == max_qtr, ]$"obsValue")
    }
  } else {
    return(pop_data[pop_data$"obsTime" == date_quarter, ]$"obsValue")
  }
}