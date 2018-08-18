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
#' \item{\code{grattan.verbose}}{Emit diagnostic messages (via \code{cat()}))}
#' }
#' 
#' 
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils OR
#' @importFrom hutils NOR
#' @importFrom hutils NEITHER
#' @importFrom hutils AND
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
#' 
#' 
#' @import data.table
#' @useDynLib grattan, .registration = TRUE
"_PACKAGE"
