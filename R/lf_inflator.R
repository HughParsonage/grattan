#' Labour force inflators
#' 
#' @param labour.force a numeric vector
#' @param from_date, to_date dates as a character vector
#' @return the relative labour force between to_date and for_date, multiplied by labour.force.

lf_inflator_fy <- function(labour_force = 1, from_fy = "2012-13", to_fy, 
                        useABSConnection = FALSE, allow.projection = TRUE, use.month = "01"){
  if (useABSConnection){
    lf.url.trend <- 
      "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1981"
    lf <- rsdmx::readSDMX(lf.url)
    lf.indices <- data.table::as.data.table(as.data.frame(lf))
  } else {
    lf.indices <- grattan:::.lf_trend
  }
  
  lf.indices <- 
    lf.indices %>%
    dplyr::mutate(obsTimeDate = as.Date(paste0(obsTime, "-01"), format = "%Y-%m-%d")) %>%
    dplyr::filter(substr(obsTime, 6, 7) == use.month) %>%
    dplyr::mutate(fy_year = date2fy(obsTimeDate))
  
  input <-
    data.table::data.table(labour_force = labour_force,
                           from_fy = from_fy,
                           to_fy = to_fy)
  
  if (!allow.projection && !all(to_fy %in% lf.indices$fy_year)){
    stop("Not all elements of to_fy are in CPI data.")
  }
  
  # Use forecast::forecast to inflate forward
  if (allow.projection && !all(to_fy %in% lf.indices$fy_year)){
    # Number of years beyond the data our forecast must reach
    years.beyond <- max(fy2yr(to_fy)) - max(fy2yr(lf.indices$fy_year))
    lf_index_forecast <- lf.indices %$% forecast::forecast(obsValue, h = years.beyond) %$% as.numeric(mean)
    lf.indices.new <- 
      data.table::data.table(fy_year = yr2fy(seq(max(fy2yr(lf.indices$fy_year)) + 1,
                                                 max(fy2yr(to_fy)),
                                                 by = 1L)),
                             obsValue = lf_index_forecast)
    lf.indices <- data.table::rbindlist(list(lf.indices, lf.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  output <- 
    input %>%
    data.table:::merge.data.table(lf.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
                                  all.x = TRUE) %>%
    dplyr::rename(from_index = obsValue) %>%
    data.table:::merge.data.table(lf.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
                                  all.x = TRUE) %>%
    dplyr::rename(to_index = obsValue) %>%
    dplyr::mutate(out = labour_force * (to_index/from_index))
  
  return(output$out)
}


lf_inflator <- function(labour.force = 1, from_date = "2013-06-30", to_date){
  
  # lf original
  lf.url <- 
    "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.10.M/ABS?startTime=1981"
  lf.url.trend <- 
    "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.6.3.1599.30.M/ABS?startTime=1981"
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

