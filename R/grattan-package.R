#' Grattan package
#' 
#' Tax modelling and other common tasks for Australian policy analysts, 
#' in support of the Grattan Institute, Melbourne. 
#' <https://grattan.edu.au>
#' 
#' @name grattan-package
#' @aliases grattanDev
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
#' \item{\code{grattan.taxstats.lib}}{Package library into which \code{taxstats} packages will
#' be installed. If \code{NULL}, a temporary directory is used.}
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
#' @importFrom hutils %notchin%
#' @importFrom hutils weighted_ntile
#' @importFrom hutils mutate_ntile
#' 
#' @importFrom hutilscpp anyOutside
#' @importFrom hutilscpp coalesce0
#' @importFrom hutilscpp isntConstant
#' @importFrom hutilscpp pmin0
#' @importFrom hutilscpp pmax0
#' @importFrom hutilscpp pmax3
#' @importFrom hutilscpp pmaxC
#' @importFrom hutilscpp pminC
#' @importFrom hutilscpp pmaxV
#' @importFrom hutilscpp pminV
#' @importFrom hutilscpp which_first


#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch
#' 
#' @importFrom fy date2fy

#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %T>%
#' @importFrom magrittr and
#' @importFrom magrittr or

#' @importFrom stats complete.cases
#' @importFrom stats setNames
#' 
#' @importFrom utils hasName
#' @importFrom utils packageName
#' 
#' 
#' @import data.table
#' @useDynLib grattanDev, .registration = TRUE
"_PACKAGE"
