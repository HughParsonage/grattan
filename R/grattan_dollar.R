#' dollar scales
#' 
#' @name grattan_dollar
#' @param x A numeric vector
#' @param digits Minimum number of digits after the decimal point. (\code{nsmall} in \code{base::format}).
#' @details Makes negative numbers appear as \eqn{-\$10,000} instead of \eqn{\$-10,000} in \code{scales::dollar}.
#' @export

# from scales
grattan_dollar <- function (x, digits = 0) {
  #
  nsmall <- digits
  commaz <- format(abs(x), nsmall = nsmall, trim = TRUE, big.mark = ",", 
                   scientific = FALSE, digits = 1L)
  
  ifelse(x < 0, 
         paste0("\U2212","$", commaz),
         paste0("$", commaz))
}
