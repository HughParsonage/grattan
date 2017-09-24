#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param family_status Family status of the individual. 
#' @export

sapto <- function(rebate_income,
                  fy.year,
                  fill = 0,
                  sapto.eligible = TRUE,
                  Spouse_income = 0,
                  family_status = "single"){
  upper_threshold <- taper_rate <- max_offset <- NULL
  input <- data.table(fy_year = fy.year, 
                      family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income)
  
  stopifnot(all(family_status %fin% c("single", "married")))
  
  if (any(Spouse_income > 0 & family_status == "single")){
    stop("family_status may be 'single' if and only if Spouse_income > 0.")
  }
  
  partner_sapto <- sapto_value <- 
    sapto_income <- partner_unused_sapto <-
    AA <- BB <- CC <- DD <- EE <- FF <-
    GG <- HH <- II <- JJ <- NULL
  
  out <- 
    sapto_tbl[input, on = c("fy_year", "family_status")] %>%
    .[, sapto_income := rebate_income + (family_status == "married") * Spouse_income] %>%
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
    
    
    # .[, FF := EE + 18200] %>% # not used
    
    # https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    .[, GG := 18200 + DD / 0.19] %>%
    # ATO calculator suggests this was intended:
    # .[, GG := 37230] %>%
    .[, HH := pmaxC(AA - GG, 0)] %>%
    .[, II := HH / 8] %>%
    .[, JJ := pmaxC(CC - II, 0)] %>% 
    .[family_status != "single",
      sapto_value := JJ] %>%
    .[family_status != "single" & rebate_income < GG,
      sapto_value := CC] %>%
    # Eligibility for SAPTO
    # my_printer %>%
    .[["sapto_value"]]
  
  sapto.eligible * coalesce(out, fill)
}