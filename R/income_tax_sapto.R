#' Income tax payable as a function of SAPTO
#' 
#' @name income_tax_sapto
#' @param income The individual assessable income.
#' @param fy.year The financial year in which the income was earned. Only tax years from 2000-01 to 2016-17 are available. If fy.year is not given, the current financial year is used by default.
#' @param age The individual's age.
#' @param family_status For Medicare and SAPTO purposes.
#' @param n_dependants An integer for the number of children of the taxpayer (for the purposes of the Medicare levy).
#' @param return.mode The mode (numeric or integer) of the returned vector.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files. If \code{.dots.ATO} is a \code{data.table}, I recommend you enclose it with \code{copy()}.
#' @param allow.forecasts should dates beyond 2016-17 be permitted? Currently, not permitted.
#' @param sapto.eligible Specify explicitly the eligibility for SAPTO. If missing, defaults to ages over 65.
#' @param medicare.sapto.eligible Specify explicitly the eligibility for SAPTO with respect to the Medicare levy for low-income earners. If missing, defaults to ages over 65.
#' @param new_sapto_tbl If not \code{NULL}, supplied to \code{\link{new_sapto}}. Otherwise, \code{fy.year} is passed to \code{\link{sapto}}.
#' @details Used to cost simple changes to SAPTO.
#' @export income_tax_sapto

income_tax_sapto <- function(income,
                             fy.year=NULL,
                             age = 42,
                             family_status = "individual",
                             n_dependants = 0L,
                             return.mode = c("numeric", "integer"),
                             .dots.ATO = NULL,
                             allow.forecasts = FALSE,
                             sapto.eligible, 
                             medicare.sapto.eligible, 
                             new_sapto_tbl = NULL) {
  if (is.null(fy.year)) {
    fy.year <- date2fy(Sys.Date())
    warning("fy.year is missing, using current financial year")
  }
  
  c_age_30_june <- NULL
  if (is.null(.dots.ATO)) {
    .dots.ATO <- data.table(ic_taxable_income_loss = income,
                            c_age_30_june = as.integer(age),
                            sp_flag = as.integer(family_status != "individual"),
                            c_depend_child = as.integer(n_dependants))
  }
  if (!missing(sapto.eligible)) {
    .dots.ATO[, c_age_30_june := fifelse(rep_len(sapto.eligible, .N), 67L, 42L)]
  }
  
  income_tax2(income, fy.year, .dots.ATO = .dots.ATO)
}


