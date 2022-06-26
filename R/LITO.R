#' Low Income Tax Offset
#' @name lito
#' @description The Low Income Tax Offset (LITO) is a non-refundable tax offset to reduce ordinary personal income tax for low-income earners.
#' 
#' N.B. Since v2.0.0, \code{lito} only calculates the actual LITO, rather than
#' an offset with custom parameters. For such functionality, use (unexported) \code{Offset}.
#' @param income The income on which the offset is applied.
#' @param fy.year The financial year for which the LITO is desired. 
#' @return
#' The LITO or LMITO for the given income and tax year.
#' @export lito lmito
NULL


#' @rdname lito
lito <- function(income, fy.year = NULL) {
  if (is.null(fy.year)) {
    fy.year <- fy::date2fy(Sys.Date())
    message("Using fy.year = ", fy.year)
  }
  Year <- fy::fy2yr(fy.year)
  .Call("C_lito", income, Year, 0L, PACKAGE = "grattan")
}

#' @rdname lito
lmito <- function(income, fy.year = NULL) {
  if (is.null(fy.year)) {
    fy.year <- fy::date2fy(Sys.Date())
    message("Using fy.year = ", fy.year)
  }
  Year <- fy::fy2yr(fy.year)
  .Call("C_lito", income, Year, 1L, PACKAGE = "grattan")
}







