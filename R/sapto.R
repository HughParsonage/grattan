#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param family_status Family status of the individual. 
#' @param on_sapto_cd SAPTO claim code type (for non-veterans). A 
#' letter A-E. A = single, B = lived apart due to illness and spouse was eligible,
#'  C = lived apart but spouse ineligible, D = lived together, both eligible for sapto, 
#'  E = lived together, spouse ineligible. Only \code{"A"} and \code{"D"} are supported.
#'  An empty string for 
#' @param .check Run checks for consistency of values. For example, ensuring no 
#' single individuals have positive \code{Spouse_income}.
#' @export

sapto <- function(rebate_income,
                  fy.year,
                  fill = 0,
                  sapto.eligible = TRUE,
                  Spouse_income = 0,
                  family_status = "single",
                  on_sapto_cd = "A",
                  .check = TRUE) {
  taper_rate <- max_offset <- lower_threshold <- NULL
  is_married <- Spouse_income > 0
  input <- data.table(fy_year = fy.year, 
                      family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income,
                      is_married = is_married)
  
  sapto_value <- 
    sapto_income <- partner_unused_sapto <-
    AA <- BB <- CC <- DD <- 
    GG <- HH <- II <- JJ <- . <- NULL
  
  out_tbl <- sapto_tbl[input, on = c("fy_year", "family_status")]
  setindex(out_tbl, is_married)
  out <- 
    out_tbl %>%
    .[, sapto_income := rebate_income + Spouse_income] %>%
    .[, sapto_value := pmax0(pminV(max_offset, 
                                   max_offset + lower_threshold * taper_rate - sapto_income * taper_rate))] %>%
    # https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    .[(is_married),
      partner_unused_sapto := pmax0(pminV(max_offset / 2,
                                          max_offset / 2 + taper_rate * lower_threshold / 2 - taper_rate * Spouse_income))] %>%
    # Transfer unutilized SAPTO:
    .[(is_married), lito := 445] %>%
    .[(is_married), AA := rebate_income] %>%
    .[(is_married), BB := max_offset / 2] %>%
    .[(is_married), CC := BB + partner_unused_sapto] %>% 
    .[(is_married), DD := CC + lito] %>%
    # .[(is_married), EE := DD / 0.19] %>%
    
    
    # .[, FF := EE + 18200] %>% # not used
    
    # https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    .[(is_married), GG := 18200 + DD / 0.19] %>%
    # ATO calculator suggests this was intended:
    # .[, GG := 37230] %>%
    .[(is_married), HH := pmax0(AA - GG)] %>%
    .[(is_married), II := HH / 8] %>%
    .[(is_married), JJ := pmax0(CC - II)] %>% 
    .[(is_married),
      sapto_value := JJ] %>%
    .[is_married & rebate_income < GG,
      sapto_value := CC] %>%
    # Eligibility for SAPTO
    # my_printer %>%
    .subset2("sapto_value")
  
  sapto.eligible * coalesce(out, fill)

}


