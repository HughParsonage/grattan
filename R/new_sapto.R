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
  stopifnot(all(family_status %chin% c("single", "married")))
  
  upper_threshold <- taper_rate <- max_offset <- NULL
  input <- data.table(family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income)
  ordering <- NULL
  input[, ordering := 1:.N]
  
  setkeyv(input, "family_status")
  
  partner_sapto <- sapto_value <- 
    sapto_income <- partner_unused_sapto <-
    AA <- BB <- CC <- DD <- EE <- FF <-
    GG <- HH <- II <- JJ <- NULL
  
  out <- 
    new_sapto_tbl[input] %>%
    .[, sapto_income := if_else(family_status == "married",
                                rebate_income + Spouse_income, 
                                rebate_income)] %>%
    .[, sapto_value := pmaxC(pminV(max_offset, 
                                   max_offset + lower_threshold * taper_rate - sapto_income * taper_rate),
                             0)] %>%
    # https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    .[, partner_unused_sapto := pmaxC(pminV(max_offset / 2,
                                            max_offset / 2 + taper_rate * lower_threshold / 2 - taper_rate * Spouse_income), 
                                      0)] %>%
    # Transfer unutilized SAPTO:
    .[, lito := 445] %>%
    .[, AA := rebate_income] %>%
    .[, BB := max_offset / 2] %>%
    .[, CC := BB + partner_unused_sapto] %>% 
    .[, DD := CC + lito] %>%
    .[, EE := DD / 0.19] %>%
    .[, FF := EE + 18200] %>%
    # https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    .[, GG := 18200 + DD / 0.19] %>%
    # ATO calculator suggests this was intended:
    # .[, GG := 37230] %>%
    .[, HH := pmaxC(AA - GG, 0)] %>%
    .[, II := HH / 8] %>%
    .[, JJ := pmaxC(CC - II, 0)] %>% 
    .[, sapto_value := if_else(family_status == "single",
                               sapto_value,
                               if_else(rebate_income < GG, CC, JJ))] %>%
    setkey(ordering) %>%
    unique(by = key(.)) %>%
    .[["sapto_value"]]
  
  # Eligibility for SAPTO
  out[!sapto.eligible] <- 0
  out[is.na(out)] <- fill
  out
}
