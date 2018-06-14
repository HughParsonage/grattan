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

weighted_ntile <- function(vector, weights = rep(1, length(vector)), n) {
  if (is.null(weights)) {
    weights <- 1
  }
  min_w <- min(weights)
  if (min_w < 0) {
    stop("`weights` contained negative values. Ensure `weights` is non-negative.")
  }
  
  if (min_w == 0) {
    warning("Some weights are zero. Maximum ntile may be incorrect.")
  }
  
  # We need to sort `vector` first before cumsumming.
  # CRAN NOTE avoidance
  vec <- wts <- orig_order <- NULL
  # 
  .weight <- 
    if (length(weights) == 1L) {
      rep(weights, length(vector))
    } else if (length(weights) == length(vector)) {
      weights
    } else {
      stop("`weights` must be length-one or length(vector).")
    }
  
  out <- 
    setDT(list(vec = vector, 
               wts = .weight,
               orig_order = seq_along(vector))) %>%
    setorderv("vec") %>%
    .[, out := as.integer(floor((n * cumsum(shift(x = wts, n = 1L, fill = 0)) / sum(wts)) + 1))] %>% 
    setorderv("orig_order") %>%
    .[["out"]]
  
  if (max(out) > n){
    warning("Some ntiles greater than n = ", n)
  } 
  out
}
