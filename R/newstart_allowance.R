#' Newstart allowance
#' 
#' @param ordinary_income 'Ordinary income' within the meaning of s. 1068-G1 of the \emph{Social Security Act 1991}. 
#' @param partner_income The partner's income.
#' @param age The individual's age. Different rates apply to those aged 18 to 20.
#' @param per Fortnight, the default, specified the units the inputs and outputs are returned.
#' @param principal_carer parent wiht most of the day to day care of child. Defined in https://www.humanservices.gov.au/individuals/enablers/principal-carer-rules-parenting-payment/41456
#' @source \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/ssa1991186/s1068.html}
#' @export newstart_allowance

#historical rates single: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #married: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
#better copy of rates: https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
newstart_allowance <- function(ordinary_income = 0,
                               has_partner = FALSE,
                               n_dependants = 0L,
                               nine_months = FALSE,
                               isjspceoalfofcoahodeoc = FALSE,
                               principal_carer = FALSE,
                               partner_income = 0,
                               partner_eligible = FALSE,
                               age = 22,
                               fy.year = "2015-16",
                               assets_value = 0,
                               homeowner = FALSE,
                               income_free_area = NULL,
                               lower = 102,
                               upper = 252,
                               taper1 = 0.5,
                               taper2 = 0.6,
                               taper3 = 0.4,
                               per = "fortnight") {
  max_rate_March_2016 <-
    if_else(isjspceoalfofcoahodeoc,
            737.10,
            if_else(has_partner,
                    476.4,
                    if_else(and(age >= 60, nine_months),
                            570.80,
                            if_else(n_dependants > 0,
                                    570.80,
                                    527.60))))
  max_income_March_2016 <-
    if_else(isjspceoalfofcoahodeoc,
            1974.75,
            if_else(has_partner,
                    934.74,
                    if_else(and(age > 60, nine_months),
                            1104.50,
                            if_else(n_dependants == 0,
                                    1021,
                                    if_else(principal_carer,
                                            1552.75,
                                            1094.17)))))
  
  income_reduction <-
    if_else(ordinary_income < lower,
            0,
            if_else(principal_carer,
                    if_else(ordinary_income < max_income_March_2016,
                            taper3 * (ordinary_income - lower),
                            max_rate_March_2016),
                    if_else(ordinary_income < upper,
                            taper1 * (ordinary_income - lower),
                            if_else(ordinary_income < max_income_March_2016,
                                    taper1 * (ordinary_income - lower) +
                                      (taper2 - taper1) * (ordinary_income - upper),
                                    max_rate_March_2016))))
  
  asset_reduction<-
    if_else(has_partner,
            if_else(homeowner,
                    if_else(assets_value > 286500,
                            max_rate_March_2016,
                            0),
                    if_else(assets_value > 433000,
                            max_rate_March_2016,
                            0)),
            if_else(homeowner,
                    if_else(assets_value > 202000,
                            max_rate_March_2016,
                            0),
                    if_else(assets_value > 348500,
                            max_rate_March_2016,
                            0)))
  
  partner_income_reduction <-#https://web.archive.org/web/20160812171654/http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
    if_else(has_partner & !partner_eligible & (partner_income > max_rate_March_2016),
            (partner_income - round((max_rate_March_2016 - (150 * 0.5)) * (1/0.6) + upper)) * 0.6 ,
            0)
    
  #output
  
  pmaxC(max_rate_March_2016 - income_reduction - asset_reduction - partner_income_reduction,
        0)
            
  
}

