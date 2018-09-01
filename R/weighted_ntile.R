#' Weighted quantiles
#' 
#' @param vector The vector for which quantiles are desired.
#' @param weights The weights associated with the vector. None should be \code{NA} or zero.
#' @param n The number of quantiles desired.
#' @return A vector of integers corresponding to the ntiles. (As in \code{dplyr::ntile}.)
#' @examples
#' weighted_ntile(1:10, n = 5)
#' weighted_ntile(1:10, weights = c(rep(4, 5), rep(1, 5)), n = 5)
#' @export
#' @details With a short-length vector, or with weights of a high variance, the results may be unexpected.

weighted_ntile <- function(vector, weights = rep(1, times = length(vector)), n) {
  if (missing(weights) || length(weights) <= 1L) {
    v <- vector
    # This line is basically from dplyr. MIT License
    return(as.integer(n * {frank(v, ties.method = "first") - 1} / length(v) + 1))
  }
  
  min_w <- min(weights)
  if (min_w < 0) {
    stop("`weights` contained negative values. Ensure `weights` is non-negative.")
  }
  
  if (min_w == 0) {
    warning("Some weights are zero. Maximum ntile may be incorrect.")
  }
  
  if (length(weights) != length(vector)) {
    stop("`weights` must be length-one or length(vector).")
  }
  
  ov <- order(vector)
  out <- as.integer(n * cumsum(shift(x = weights[ov], fill = 0)) / sum(weights))
  
  if (last(out) >= n) {
    warning("Some ntiles greater than n = ", n)
  } 
  out[order(ov)] + 1L
}


