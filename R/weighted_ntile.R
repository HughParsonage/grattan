#' Weighted quantiles
#' 
#' @param vector The vector for which quantiles are desired.
#' @param weights The weights associated with the vector. None should be \code{NA} or zero.
#' @param n The number of quantiles desired.
#' @return A vector of integers corresponding the ntiles.
#' @export

weighted_ntile <- function(vector, weights = rep(1, length(vector)), n){
  are_zero <- function(x){
    x < .Machine$double.eps ^ 0.5
  }
  
  if (any(weights %>% are_zero)){
    warning("Some weights are zero. Maximum ntile may be incorrect.")
  }
  
  ooo <- rank(vector, ties.method = "first", na.last = "keep")
  out <- floor((n *(cumsum(dplyr::lag(weights[ooo], default = 0))[ooo] / sum(weights))) + 1)
  if (any(out > n)){
    warning("Some ntiles greater than n = ", n)
  } else {
    return(out)
  }
}
