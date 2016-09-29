#' @title English prose for small numbers
#' 
#' @description When a small number is requested inline, it is often preferable to use an English expression rather than the numeral.
#' Further, referential purity can be lost when numbers are transformed to suit printing (say by multiplying by 100 to use 'per cent').
#' A complement to \code{\link{texNum}}. 
#' 
#' These functions are all of the form \code{grattan_*} to avoid namespace clashes.
#' 
#' @param number A length-one numeric vector.
#' @param digits Passed to \code{scales::comma}.
#' @param .percent.suffix What to use for the suffix of the number.
#' @return For \code{grattan_percent}, the number multiplied by 100, with rounding provided by \code{scales::comma} and then the suffix. 
#' 
#' @export grattan_percent

grattan_percent <- function(number, digits = 1, .percent.suffix = "~per~cent"){
  paste0(scales::comma(100 * number, digits = digits), .percent.suffix)
}
