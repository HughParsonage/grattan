#' Australia's population
#' 
#' @param date as character string(YYYY-QQ)
#' @return the population at \code{date}
#' 

aus_pop_qtr <- function(date_quarter){
  pop_data <- dplyr::select(data.table::data.table(as.data.frame(rsdmx::readSDMX("http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/ERP_QUARTERLY/1.0.3.TT.Q/ABS?startTime=1981"))), obsTime, obsValue)
  return(pop_data[pop_data$obsTime == date_quarter, ]$obsValue)
}