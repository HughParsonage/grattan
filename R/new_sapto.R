#' SAPTO with user-defined thresholds
#' 
#' @param rebate_income The rebate income of the individual.
#' @param new_sapto_tbl Having the same columns as \code{grattan:::sapto_tbl}, keyed on family_status.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param family_status Family status of the individual. 
#' @export

new_sapto <- function(rebate_income,
                      new_sapto_tbl, 
                      sapto.eligible = TRUE,
                      Spouse_income = 0,
                      fill = 0,
                      family_status = "single"){
  upper_threshold <- taper_rate <- max_offset <- NULL
  input <- data.table(fy_year = fy.year, 
                      family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income)
  ordering <- NULL
  input[, ordering := 1:.N]
  
  setkeyv(input, "family_status")
  
  sapto_value <- partner_sapto <- NULL
  
  out <- 
    new_sapto_tbl[input] %>%
    .[, sapto_value := pmaxC(pminV(max_offset, 
                                   max_offset + lower_threshold * taper_rate - rebate_income * taper_rate),
                             0)] %>%
    .[, partner_sapto := pmaxC(pminV(max_offset, 
                                     max_offset + lower_threshold * taper_rate - rebate_income * taper_rate),
                               0)] %>%
    # Transfer unutilized SAPTO:
    .[, sapto_value := sapto_value + pminC(max_offset - (family_status != "single") * partner_sapto, 0)] %>%
    setkey(ordering) %>%
    unique(by = key(.)) %>%
    # my_printer %>%
    .[["sapto_value"]]
  
  # Eligibility for SAPTO
  out[!sapto.eligible] <- 0
  out[is.na(out)] <- fill
  out
}
