#' Grattan package
#' 
#' Tax modelling and other common tasks for Australian policy analysts, 
#' in support of the Grattan Institute, Melbourne. 
#' <https://grattan.edu.au>
#' 
#' @name grattan-package
#' @aliases grattan
#' @docType package
#' @title The grattan package.
#' @author \email{hugh.parsonage+grattanpackage@@grattan.edu.au}
#' @author \email{hugh.parsonage@@gmail.com}
#' @keywords package
#' 
#' @section Package options:
#' \describe{
#' \item{\code{grattan.verbose}}{(\code{FALSE}) Emit diagnostic messages (via \code{cat()}))}
#' \item{\code{grattan.assume1901_2100}}{(\code{TRUE}) Assume \code{yr2fy} receives an integer >= 1901 and <= 2100.}
#' }
#' 
#' 
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils OR
#' @importFrom hutils NOR
#' @importFrom hutils NEITHER
#' @importFrom hutils AND
#' @importFrom hutils XOR
#' @importFrom hutils %ein%

#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch

#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %T>%
#' @importFrom magrittr and
#' @importFrom magrittr or

#' @importFrom Rcpp sourceCpp
#' @importFrom stats complete.cases
#' @importFrom stats setNames
#' 
#' 
#' @import data.table
#' @useDynLib grattan, .registration = TRUE
"_PACKAGE"
