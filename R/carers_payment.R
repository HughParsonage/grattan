#' Carers payment
#' @param Date,fy.year The timing of the payment.
#' @return The carer's payment, if eligible.
#' @export

carers_payment <- function(Date = NULL,
                           fy.year = NULL) {
  carers_tbl <- readRDS(system.file("extdata", "carers_tbl.rds", package = "grattan"))
  if (!is.null(fy.year)) {
    input <- setDT(list(fy.year = fy.year))
    stopifnot("Date" %in% names(carers_tbl))
    return(carers_tbl[, fy.year := date2fy(Date)][input, on = "fy.year"][, CarersPayment])
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
    return(carers_tbl[input, roll = Inf][order(ord), CarersPayment])
  }
}


