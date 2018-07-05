#' Newstart allowance
#' 
#' @param ordinary_income 'Ordinary income' within the meaning of s. 1068-G1 of the \emph{Social Security Act 1991}. 
#' @param has_partner Does the individual have a partner?
#' @param n_dependants How many dependant children does the individual have?
#' @param nine_months If the person is over 60 years old, have they been receiving payments for over 9 continuous months?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?
#' @param principal_carer parent wiht most of the day to day care of child. Defined in https://www.humanservices.gov.au/individuals/enablers/principal-carer-rules-parenting-payment/41456
#' @param partner_income The partner's income.
#' @param age The individual's age. Different rates apply to those aged 18 to 20.
#' @param fy.year Financial year. Default is "2015-16".
#' @param assets_value Total value of household assets. Details can be found at https://www.humanservices.gov.au/individuals/enablers/assets/30621 
#' @param homeowner Is the individual a homeowner?
#' @param taper1 The amount at which the payment is reduced for each dollar earned between the lower and upper bounds for non-principal carers.
#' @param taper2 The amount at which the payment is reduced for each dollar earned above the upper bound for non-principal carers.
#' @param taper3 The amount at which the payment is reduced for each dollar earned above the lower bound for principal carers.
#' @param lower Lower bound for which reduction in payment occurs at rate taper1 (taper 3 for principal carers).
#' @param upper Upper bound for which reduction in payment occurs at rate taper1. Lower bound for which reduction in payment occurs at rate taper2. Note that for principal carers there is no upper bound upper.
#' @param per Fortnight, the default, specified the units the inputs and outputs are returned.
#' @source \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/ssa1991186/s1068.html}
#' @export newstart_allowance

#historical rates single: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #married: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
#better copy of rates and reductions: https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
newstart_allowance <- function(ordinary_income = 0,
                               has_partner = FALSE,
                               n_dependants = 0,
                               nine_months = FALSE,
                               isjspceoalfofcoahodeoc = FALSE,
                               principal_carer = FALSE,
                               partner_income = 0,
                               age = 22,
                               fy.year = NULL,
                               assets_value = 0,
                               homeowner = FALSE,
                               lower = 102,
                               upper = 252,
                               taper1 = 0.5,
                               taper2 = 0.6,
                               taper3 = 0.4,
                               per = "fortnight") {
  
  if (is.null(fy.year)) {
    fy.year <- "2015-16"
    message('`fy.year` not set, so defaulting to fy.year = "2015-16"')
  }
  
  input <- data.table(do.call(cbind.data.frame, mget(ls())))
 
  max_rate_March_2016 <-
    input[ ,if_else(isjspceoalfofcoahodeoc,
                     737.10,
                     if_else(has_partner,
                             476.4,
                             if_else(and(age >= 60, nine_months),
                                     570.80,
                                     if_else(n_dependants > 0,
                                             570.80,
                                             527.60))))]
  
  #additional eligibility requirements at https://www.humanservices.gov.au/individuals/services/centrelink/newstart-allowance/who-can-get-it
  eligibility <-
    input[ ,if_else(22 <= age & age < 65,
                    0,
                    max_rate_March_2016)]
    
  max_income_March_2016 <-
    input[ ,if_else(isjspceoalfofcoahodeoc,
                    1974.75,
                    if_else(has_partner,
                            934.74,
                            if_else(and(age > 60, nine_months),
                                    1104.50,
                                    if_else(n_dependants == 0,
                                            1021,
                                            if_else(principal_carer,
                                                    1552.75,
                                                    1094.17)))))]

  income_reduction <-
    input[ ,if_else(ordinary_income < lower,
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
                                              max_rate_March_2016))))]

  asset_reduction<-
    input[ ,if_else(has_partner,
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
                                    0)))]

  partner_income_reduction <- #https://web.archive.org/web/20160812171654/http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
    if_else(has_partner & (partner_income > max_rate_March_2016),
            (partner_income - round((max_rate_March_2016 - (150 * 0.5)) * (1/0.6) + upper)) * 0.6 ,
            0)

  #output

  pmaxC(max_rate_March_2016 - eligibility - income_reduction - asset_reduction - partner_income_reduction,
        0)
}
