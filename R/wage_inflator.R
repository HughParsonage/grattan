#' a wage inflator
#' 
#' @param wage the amount to be inflated (1 by default)
#' @param from_fy a character string of the form "2012-13" representing the FY ending that the wage index is to be taken (i.e. Q4 in that year). FY year must be 1996-97 or later.
#' @param to_fy the FY ending that the wage index is to be taken
#' @return the wage inflation between the two years

wage_inflator <- function(wage = 1, from_fy, to_fy){
  wage.url <- "http://stat.abs.gov.au/restsdmx/sdmx.ashx/GetData/LABOUR_PRICE_INDEX/1.THRPEB.7.-.0.30.Q/ABS?startTime=1997"
  if(require(rsdmx)) {
    wages <- rsdmx::readSDMX(wage.url)
    message("Using ABS sdmx connection")
    wages <- as.data.frame(wages)
  } else {
    wages <- structure(list(obsTime = c("1997-Q3", "1997-Q4", "1998-Q1", "1998-Q2","1998-Q3", "1998-Q4", "1999-Q1", "1999-Q2", "1999-Q3", "1999-Q4", "2000-Q1", "2000-Q2", "2000-Q3", "2000-Q4", "2001-Q1", "2001-Q2", "2001-Q3", "2001-Q4", "2002-Q1", "2002-Q2", "2002-Q3", "2002-Q4", "2003-Q1", "2003-Q2", "2003-Q3", "2003-Q4", "2004-Q1", "2004-Q2", "2004-Q3", "2004-Q4", "2005-Q1", "2005-Q2", "2005-Q3", "2005-Q4", "2006-Q1", "2006-Q2", "2006-Q3", "2006-Q4", "2007-Q1", "2007-Q2", "2007-Q3", "2007-Q4", "2008-Q1", "2008-Q2", "2008-Q3", "2008-Q4", "2009-Q1", "2009-Q2", "2009-Q3", "2009-Q4", "2010-Q1", "2010-Q2", "2010-Q3", "2010-Q4", "2011-Q1", "2011-Q2", "2011-Q3", "2011-Q4", "2012-Q1", "2012-Q2", "2012-Q3", "2012-Q4", "2013-Q1", "2013-Q2", "2013-Q3", "2013-Q4", "2014-Q1", "2014-Q2", "2014-Q3", "2014-Q4", "2015-Q1"), obsValue = c(66.7, 67.2, 67.8, 68.3, 68.8, 69.4, 69.9, 70.4, 70.9, 71.4, 71.9, 72.5, 73.1, 73.8, 74.5, 75.1, 75.7, 76.3, 76.9, 77.5, 78.2, 78.9, 79.6, 80.3, 81.1, 81.8, 82.5, 83.2, 83.9, 84.8, 85.7, 86.5, 87.4, 88.3, 89.2, 90.1, 91, 91.9, 92.8, 93.7, 94.7, 95.6, 96.6, 97.7, 98.7, 99.7, 100.6, 101.4, 101.8, 102.6, 103.5, 104.5, 105.5, 106.6, 107.5, 108.5, 109.4, 110.4, 111.5, 112.4, 113.4, 114.2, 115, 115.7, 116.5, 117.2, 118, 118.7, 119.4, 120.1, 120.7)), row.names = c(NA, 71L), class = "data.frame", .Names = c("obsTime", "obsValue"))
  warning("Using hardcoded wage data")
  }
  
  from_fy_year <- 1 + as.numeric(gsub("^.*([12][0-9]{3}).*$", "\\1", from_fy)) 
  from_fy_as_quarter <- paste0(from_fy_year, "-", "Q4")
  
  to_fy_year <- 1 + as.numeric(gsub("^.*([12][0-9]{3}).*$", "\\1", to_fy)) 
  to_fy_as_quarter <- paste0(to_fy_year, "-", "Q4")
  
  wage * 
    wages[wages$obsTime == to_fy_as_quarter, ]$obsValue / 
      wages[wages$obsTime == from_fy_as_quarter, ]$obsValue

}