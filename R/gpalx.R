#' The extended colour palette
#' 
#' @name gpalx
#' @param n a positive integer
#' @details This function interpolates the original Grattan palette for a large (possibly infinite) number of factors.  
#' @return A vector of n HTML colours.

gpalx <- function(n){
  stopifnot(requireNamespace("grattan", quietly = TRUE))
  grDevices::colorRampPalette(colors = grattan::pal.6)(n)
}