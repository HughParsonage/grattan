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
#' @param hedges A length-2 character vector giving the words for a hedge (see \url{https://en.wikipedia.org/wiki/Hedge_(linguistics)}), 
#' included not to mislead but to sound natural without being inaccurate. 
#' If \code{NULL}, the nearest fraction regardless is used.
#' @return For \code{grattan_percent}, the number multiplied by 100, with rounding provided by \code{scales::comma} and then the suffix. 
#' 
#' For \code{Grattan_frac} and \code{grattan_frac}, the nearest fraction as an English word, sentence-case or lower-case respectively. 
#' If \code{hedges} is supplied, a phrase including the next fraction above or below it as the phrase dictates.
#' 
#' @export grattan_percent Grattan_frac grattan_frac

grattan_percent <- function(number, digits = 1, .percent.suffix = "~per~cent"){
  paste0(scales::comma(100 * number, digits = digits), .percent.suffix)
}


Grattan_frac <- function(number, hedges = c("Almost", "Over")){
  stopifnot(length(number) == 1L, abs(number) <= 1)
  
  if (is.null(hedges)){
    .avbl_fractions %>%
      .[which.min(abs(val - number))] %>%
      .[["Txt"]]
  } else {
    the_val <- 
      .avbl_fractions %>%
      .[which.min(abs(val - number))] %>%
      .[["val"]]
    if (number < the_val){
      sprintf("%s %s", hedges[1], .avbl_fractions %>% .[which.min(abs(val - number))] %>% .[["txt"]])
    } else {
      sprintf("%s %s", hedges[2], .avbl_fractions %>% .[which.min(abs(val - number))] %>% .[["txt"]])
    }
  }
}

grattan_frac <- function(number, hedges = c("Almost", "Over")){
  tolower(Grattan_frac(number = number, pre.phrase = pre.phrase))
} 

  