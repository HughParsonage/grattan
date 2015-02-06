#' The extended colour palette
#' 
#' @name gpalx
#' @param n a positive integer
#' @details This function interpolates the original Grattan palette for a large (possibly infinite) number of factors.  
#' @return A vector of n HTML colours.

gpalx <- function(n) grDevices::colorRampPalette(colors = pal.6)(n)