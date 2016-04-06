#' a wage inflator
#' 
#' @param wage the amount to be inflated (1 by default)
#' @param from_fy a character vector of the form "2012-13" representing the FY ending that the wage index is to be taken (i.e. Q4 in that year). FY year must be 1996-97 or later.
#' @param to_fy the FY ending that the wage index is to be taken
#' @param useABSConnection Should the function connect with ABS.Stat via an SDMX connection? By default set to \code{FALSE} in which case a pre-prepared index table is used. This is much faster and more reliable (in terms of errors), though of course relies on the package maintainer to keep the tables up-to-date.
#' @param allow.projection If set to \code{TRUE} the \code{forecast} package is used to project forward. If set to \code{FALSE}, any dates outside the range will return \code{NA}.
#' @return the wage inflation between the two years

wage_inflator <- function(wage = 1, from_fy, to_fy, useABSConnection = FALSE, allow.projection = TRUE){
  
  # CRAN
  obsTime <- NULL; obsValue <- NULL; to_index <- NULL; from_index <- NULL
  
  if (any(is.na(from_fy)) || any(is.na(to_fy))){
    stop("from_fy and to_fy contain NAs. Filter before applying this function.")
  }
  # Avoid vector recycling
  prohibit_vector_recycling(wage, from_fy, to_fy)
  
  if(useABSConnection) {
    wage.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/ABS?startTime=1997"
    wages <- rsdmx::readSDMX(wage.url)
    message("Using ABS sdmx connection")
    wage.indices <- as.data.frame(wages)
  } else {
    # .wages_trend means the wage indices of the trend index
    wage.indices <- wages_trend
  }
  
  wage.indices <- 
    data.table::as.data.table(wage.indices) %>%
    dplyr::filter(grepl("Q1", obsTime)) %>%
    dplyr::mutate(fy_year = yr2fy(sub("-Q1", "", obsTime, fixed = TRUE)))
  
  if(!allow.projection && !all(to_fy %in% wage.indices$fy_year)){
    stop("Not all elements of to_fy are in wage index data.")
  }
  # else allow NAs to propagate
  
  # Use forecast::forecast to inflate forward
  if(allow.projection && isTRUE(!all(to_fy %in% wage.indices$fy_year))){
    # Number of years beyond the data our forecast must reach
    years.beyond <- max(fy2yr(to_fy)) - max(fy2yr(wage.indices$fy_year))
    wage_index_forecast <- wage.indices %$% forecast::forecast(obsValue, h = years.beyond) %$% as.numeric(mean)
    wage.indices.new <- 
      data.table::data.table(fy_year = yr2fy(seq(max(fy2yr(wage.indices$fy_year)) + 1,
                                                     max(fy2yr(to_fy)),
                                                     by = 1L)),
                             obsValue = wage_index_forecast)
    wage.indices <- data.table::rbindlist(list(wage.indices, wage.indices.new), use.names = TRUE, fill = TRUE)
  }
  
  input <-
    data.table::data.table(wage = wage,
                           from_fy = from_fy,
                           to_fy = to_fy)
  
  
  
  output <- 
    input %>%
    merge(wage.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE, all.x = TRUE) %>%
    dplyr::rename(from_index = obsValue) %>%
    merge(wage.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, all.x = TRUE) %>%
    dplyr::rename(to_index = obsValue) %>%
    dplyr::mutate(out = wage * (to_index/from_index)) 
  
  
  
#   wage * 
#     wages[wages$obsTime == to_fy_as_quarter, ]$obsValue / 
#       wages[wages$obsTime == from_fy_as_quarter, ]$obsValue
  output$out
}