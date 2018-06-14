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
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils OR
#' @importFrom hutils NEITHER
#' @importFrom hutils AND

#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch

#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom magrittr and

#' @importFrom Rcpp sourceCpp
#' @importFrom stats complete.cases
#' 
#' 
#' @import data.table
#' @useDynLib grattan, .registration = TRUE
"_PACKAGE"
