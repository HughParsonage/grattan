#' Ensure equilength, unempty integer vectors
#' @noRd
#' @param x An atomic vector.
#' @return
#' Replaces missing values with 0, and replicates length-one vectors to the
#' required length. Numeric vectors are capped at 2147483647 then converted
#' to integer.
#' 
#' 
#' 


do_rN <- .rN <- function(x, zero, nThread = getOption("grattan.nThread", 1L)) {
  if (is.integer(x) && length(x) == length(zero) && !anyNA(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.integer(coalesce0(x)))
  }
  .Call("Cdo_rn", x, zero, nThread, PACKAGE = utils::packageName())
}
