#' Carers allowance
#' @param Date,fy.year The timing of the allowance.
#' @param per Frequency of the payment. 
#' @return The carer's payment, if eligible.
#' @export

carers_allowance <- function(Date = NULL,
                           fy.year = NULL, 
                           per = c("year", "fortnight")) {
  carers_tbl <- readRDS(system.file("extdata", "carers_tbl.rds", package = "grattan"))
  # per_m <- validte_per(per, missing(per))
  
  out <- 
    if (!is.null(fy.year)) {
      input <- setDT(list(fy.year = fy.year))
      stopifnot("Date" %in% names(carers_tbl))
      carers_tbl[, fy.year := date2fy(Date)][input, on = "fy.year"][, CarersPayment]
    } else {
      if (is.null(Date)) {
        Date <- Sys.Date()
        message("`fy.year` and `Date` not set, so setting `Date = ",
                as.character(Date), "`.")
      }
      input <- setDT(list(Date = Date, 
                          ord = seq_along(Date)))
      setkeyv(input, "Date")
      setkeyv(carers_tbl, "Date")
      carers_tbl[input, roll = Inf][order(ord), CarersPayment]
    }
  out
}


