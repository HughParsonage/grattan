#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param family_status Family status of the individual. 
#' @param .check Run checks for consistency of values. For example, ensuring no 
#' single individuals have positive \code{Spouse_income}.
#' @export

sapto <- function(rebate_income,
                  fy.year,
                  fill = 0,
                  sapto.eligible = TRUE,
                  Spouse_income = 0,
                  family_status = "single", 
                  .check = TRUE) {
  taper_rate <- max_offset <- lower_threshold <- NULL
  is_married <- Spouse_income > 0
  input <- data.table(fy_year = fy.year, 
                      family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income,
                      is_married = is_married)
  
  if (.check) {
    # Unit: microseconds
    #                                                  expr      min       lq     mean
    #    any(Spouse_income > 0 & family_status == "single") 3683.391 4405.311 6068.063
    #  any(Spouse_income * (family_status == "single") > 0) 3260.539 3857.170 4785.296
    #     any(Spouse_income[family_status == "single"] > 0) 3390.648 4144.794 5092.755
    #     any(family_status[Spouse_income > 0] == "single") 3067.786 3608.248 4552.807
    #       "single" %fin% family_status[Spouse_income > 0] 3056.643 3484.163 4295.382
    #      "single" %chin% family_status[Spouse_income > 0] 3043.390 3629.330 4675.723
    #                   "single" %chin% family_status[sip0] 2461.216 2836.934 3474.478
    #                              sip <- Spouse_income > 0  674.938  815.888 1032.746
    #                      fms <- family_status == "single" 1706.769 2020.293 2601.043
    
    
    stopifnot(all(family_status %chin% c("single", "married")))
    
    if ("single" %chin% family_status[is_married]) {
      if (length(family_status) == 1L) {
        stop("`family_status = 'single'` yet `Spouse_income > 0`. ", 
             "Whenever `Spouse_income` is positive, `family_status` must be set to 'married'.")
      } else {
        first_bad <- which(family_status == "single" & is_married)
        stop("In entry ", first_bad, " `family_status = 'single' ", 
             "yet Spouse_income > 0 for that entry. ",
             "Whenever `Spouse_income` is positive, `family_status` must be set to 'married'.")
      }
    }
  }
  
  sapto_value <- 
    sapto_income <- partner_unused_sapto <-
    AA <- BB <- CC <- DD <- 
    GG <- HH <- II <- JJ <- . <- NULL
  
  out_tbl <- sapto_tbl[input, on = c("fy_year", "family_status")]
  setindex(out_tbl, is_married)
  out <- 
    out_tbl %>%
    .[, sapto_income := rebate_income + Spouse_income] %>%
    .[, sapto_value := pmaxIPnum0(pminV(max_offset, 
                                        max_offset + lower_threshold * taper_rate - sapto_income * taper_rate))] %>%
    # https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    .[(is_married),
      partner_unused_sapto := pmaxIPnum0(pminV(max_offset / 2,
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
    .[(is_married), HH := pmaxIPnum0(AA - GG)] %>%
    .[(is_married), II := HH / 8] %>%
    .[(is_married), JJ := pmaxIPnum0(CC - II)] %>% 
    .[(is_married),
      sapto_value := JJ] %>%
    .[is_married & rebate_income < GG,
      sapto_value := CC] %>%
    # Eligibility for SAPTO
    # my_printer %>%
    .subset2("sapto_value")
  
  sapto.eligible * coalesce(out, fill)

}

