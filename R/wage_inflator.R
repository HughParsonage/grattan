#' a wage inflator
#' 
#' @param wage the amount to be inflated (1 by default)
#' @param from_fy a character vector of the form "2012-13" representing the FY ending that the wage index is to be taken (i.e. Q4 in that year). FY year must be 1996-97 or later.
#' @param to_fy the FY ending that the wage index is to be taken
#' @return the wage inflation between the two years

wage_inflator <- function(wage = 1, from_fy, to_fy, useABSConnection = FALSE){
  wage.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/ABS?startTime=1997"
  if(useABSConnection) {
    wages <- rsdmx::readSDMX(wage.url)
    message("Using ABS sdmx connection")
    wage.indices <- as.data.frame(wages)
  } else {
    wage.indices <- grattan:::.wages_trend
  }
  
  wage.indices <- 
    data.table::as.data.table(wage.indices) %>%
    dplyr::filter(grepl("Q1", obsTime)) %>%
    dplyr::mutate(fy_year = yr2fy(sub("-Q1", "", obsTime, fixed = TRUE)))
  
  input <-
    data.table::data.table(wage = wage,
                           from_fy = from_fy,
                           to_fy = to_fy)
  
  output <- 
    input %>%
    data.table:::merge.data.table(wage.indices, by.x = "from_fy", by.y = "fy_year", sort = FALSE,
                                  all.x = TRUE) %>%
    dplyr::rename(from_index = obsValue) %>%
    data.table:::merge.data.table(wage.indices, by.x = "to_fy", by.y = "fy_year", sort = FALSE, 
                                  all.x = TRUE) %>%
    dplyr::rename(to_index = obsValue) %>%
    dplyr::mutate(out = wage * (to_index/from_index)) 
  
#   wage * 
#     wages[wages$obsTime == to_fy_as_quarter, ]$obsValue / 
#       wages[wages$obsTime == from_fy_as_quarter, ]$obsValue
  output$out
}