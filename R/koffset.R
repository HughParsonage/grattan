
koffset <- function(income, X, Y) {
  stats::approxfun(x = X, y = Y)(income)
}




