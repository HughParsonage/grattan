#' Any without logical creation
#' @param x An integer vector.
#' @param a An integer.
#' @return 0 if none true or the index of the first match.
#' 
#' 

anyGeq <- function(x, a) {
  AnyWhich(x, a,
           # greater
           TRUE, 
           # less than
           FALSE,
           # equal to
           TRUE)
}

anyGt <- function(x, a) {
  AnyWhich(x, a,
           # greater
           TRUE, 
           # less than
           FALSE,
           # equal to
           FALSE)
}

anyLeq <- function(x, a) {
  AnyWhich(x, a,
           # greater
           FALSE, 
           # less than
           TRUE,
           # equal to
           TRUE)
}

anyLt <- function(x, a) {
  AnyWhich(x, a,
           # greater
           FALSE, 
           # less than
           TRUE,
           # equal to
           FALSE)
}

anyEqual <- function(x, a) {
  AnyWhich(x, a,
           # greater
           FALSE, 
           # less than
           FALSE,
           # equal to
           TRUE)
}

anyNotEqual <- function(x, a) {
  AnyWhich(x, a,
           # greater
           FALSE, 
           # less than
           FALSE,
           # equal to
           FALSE)
}

