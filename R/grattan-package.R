#' Grattan package
#' 
#' Colours, charts, and other common quantative tasks for Australia tax analysts
#' 
#' @name grattan-package
#' @aliases grattan
#' @docType package
#' @title The grattan package.
#' @author \email{hugh.parsonage+grattanpackage@@grattan.edu.au}
#' @keywords package
#' @importFrom hutils if_else
#' @importFrom hutils OR
#' @importFrom hutils NEITHER
#' @importFrom hutils AND

#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom magrittr and

#' @importFrom Rcpp sourceCpp
#' @importFrom stats complete.cases
#' @import data.table
#' @useDynLib grattan, .registration = TRUE
"_PACKAGE"
