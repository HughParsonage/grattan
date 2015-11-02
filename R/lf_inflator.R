#' Labour force inflators
#' 
#' @param labour.force a numeric vector
#' @param from_date, to_date dates as a character vector
#' @return the relative labour force between to_date and for_date, multiplied by labour.force.


lf_inflator <- function(labour.force = 1, from_date = "2013-06-30", to_date){
  
  lf.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.10.M/ABS?startTime=1981"
  lf <- rsdmx::readSDMX(lf.url)
  lf <- as.data.frame(lf)
  lf$obsTimeDate <- as.Date(paste0(lf$obsTime, "-01"), format = "%Y-%m-%d")
  
  if (length(labour.force) == 1){
    if(is.na(labour.force))
      return(NA)
    else {
      nearest_from_date <- as.Date(max(lf$obsTimeDate[lf$obsTimeDate <= from_date]))
      if(difftime(from_date, nearest_from_date, units = "days") > 182)
        warning("From dates differ by more than 182 days")
      
      nearest_to_date <- as.Date(max(lf$obsTimeDate[lf$obsTimeDate <= to_date]))
      if(difftime(to_date, nearest_to_date, units = "days") > 182)
        warning("To dates differ by more than 182 days")
      
      from_labour.force <- lf$obsValue[lf$obsTimeDate == nearest_from_date]
      to_labour.force <- lf$obsValue[lf$obsTimeDate == nearest_to_date]
      #
      #
      return(labour.force * to_labour.force / from_labour.force)
    }
  } else {
    date_connector <- 
      data.table::data.table(
        altkey = 1:length(from_date),
        from_date = as.Date(from_date),
        to_date = as.Date(to_date)
      )
    
    LF <- data.table::data.table(
      obsTimeDate = lf$obsTimeDate,
      obsValue = lf$obsValue
    )
    data.table::setkey(LF, obsTimeDate)
    
    data.table::setkey(date_connector, from_date)
    from_DT <- LF[date_connector, roll=Inf]
    data.table::setnames(from_DT, c("obsTimeDate", "obsValue"), c("from_Date", "from_LF"))
    
    data.table::setkey(date_connector, to_date)
    to_DT <- LF[date_connector, roll=Inf]
    data.table::setnames(to_DT, c("obsTimeDate", "obsValue"), c("to_Date", "to_LF"))
    
    merged <- data.table:::merge.data.table(from_DT, to_DT, by = "altkey")
    lf.ratio <- labour.force * merged$to_LF / merged$from_LF
    return(lf.ratio)
  }
}

