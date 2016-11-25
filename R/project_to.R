#' A function for simple projections of sample files
#' 
#' @param sample_file A sample file, most likely the 2012-13 sample file. It is intended that to be the most recent.
#' @param to_fy A string like "1066-67" representing the financial year for which forecasts of the sample file are desired. 
#' @param fy.year.of.sample.file The financial year of \code{sample_file}.
#' @param ... Other arguments passed to \code{\link{project}}.
#' @return A sample file of the same number of rows as \code{sample_file} with inflated values (including WEIGHT).
#' @export

project_to <- function(sample_file, to_fy, fy.year.of.sample.file = "2013-14", ...){
  h <- as.integer(fy2yr(to_fy) - fy2yr(fy.year.of.sample.file))
  project(sample_file = sample_file, h = h, fy.year.of.sample.file = fy.year.of.sample.file, ...)
}