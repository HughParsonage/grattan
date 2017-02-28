#' New SAPTO thresholds
#' 
#' @param rebate_income The rebate income of the individual.
#' @param new_sapto_tbl Having the same columns as \code{grattan:::sapto_tbl}, keyed on family_status.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param family_status Family status of the individual.
#' @export

new_sapto <- function(rebate_income,
                      new_sapto_tbl, 
                      sapto.eligible = TRUE,
                      fill = 0,
                      family_status = "single"){
  upper_threshold <- taper_rate <- max_offset <- NULL
  input <- data.table(family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income)
  ordering <- NULL
  input[, ordering := 1:.N]
  
  setkeyv(input, "family_status")
  
  out <- 
    new_sapto_tbl[input] %>%
    .[, sapto := pmaxC(pminV(max_offset, 
                             upper_threshold * taper_rate - rebate_income * taper_rate),
                       0)] %>%
    setkey(ordering) %>%
    unique %>%
    .[["sapto"]]
  
  # Eligibility for SAPTO
  out[!sapto.eligible] <- 0
  out[is.na(out)] <- fill
  out
}
