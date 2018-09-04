
koffset <- function(income, X, Y, Yright, Method = "linear") {
  stats::approxfun(x = X, y = Y, yright = Yright, method = Method)(income)
}




