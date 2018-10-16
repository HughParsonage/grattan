#' Newstart allowance
#' 
#' @param fortnightly_income 'Ordinary income' received fortnightly within the 
#' meaning of s. 1068-G1 of the \emph{Social Security Act 1991}. 
#' @param annual_income 'Ordinary income' received annually.
#' @param has_partner Does the individual have a partner?
#' @param partner_pensioner Does the partner receive a pension?
#' @param n_dependants How many dependant children does the individual have?
#' @param nine_months If the person is over 60 years old, have they been 
#' receiving payments for over 9 continuous months?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?
#' @param principal_carer Is the individual the parent with most of the day-to-day
#'  care of child. Defined in \url{https://www.humanservices.gov.au/individuals/enablers/principal-carer-rules-parenting-payment/41456}.
#' @param fortnightly_partner_income Partner's 'Ordinary income' received fortnightly. 
#' @param annual_partner_income Partner's Ordinary income' received annually.
#' @param age The individual's age.
#' @param fy.year Financial year. Default is "2015-16".
#' @param assets_value Total value of household assets. Details can be found at
#' \url{https://www.humanservices.gov.au/individuals/enablers/assets/30621}. 
#' @param homeowner Is the individual a homeowner?
#' @param taper_lower The amount at which the payment is reduced for each dollar
#'  earned between the lower and upper bounds for non-principal carers.
#' @param taper_upper The amount at which the payment is reduced for each dollar
#'  earned above the upper bound for non-principal carers.
#' @param taper_principal_carer The amount at which the payment is reduced for
#'  each dollar earned above the lower bound for principal carers.
#' @param lower Lower bound for which reduction in payment occurs at rate 
#' \code{taper_lower} (\code{taper_principal_carer} for principal carers).
#' @param upper Upper bound for which reduction in payment occurs at rate 
#' \code{taper_lower}. Lower bound for which reduction in payment occurs at rate
#' \code{taper_upper}. Note that for principal carers there is no upper bound.
#' @param per Specifies the timeframe in which payments will be made. 
#' Can either take value "fortnight" or "annual".
#' @source \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/ssa1991186/s1068.html}
#' @export newstart_allowance

#historical rates single: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #married: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
#better copy of rates and reductions: https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
newstart_allowance <- function(fortnightly_income = 0,
                               annual_income = 0,
                               has_partner = FALSE,
                               partner_pensioner = FALSE,
                               n_dependants = 0,
                               nine_months = FALSE,
                               isjspceoalfofcoahodeoc = FALSE,
                               principal_carer = FALSE,
                               fortnightly_partner_income = 0,
                               annual_partner_income = 0,
                               age = 22,
                               fy.year = "2015-16",
                               assets_value = 0,
                               homeowner = FALSE,
                               lower = 102,
                               upper = 252,
                               taper_lower = 0.5,
                               taper_upper = 0.6,
                               taper_principal_carer = 0.4,
                               per = c("year", "fortnight")) {
  
  if (!identical(fy.year, "2015-16")) {
    stop('`fy.year` can only take value "2015-16" for now')
  }
  
  prohibit_vector_recycling(fortnightly_income,
                            annual_income,
                            has_partner,
                            partner_pensioner,
                            n_dependants,
                            nine_months,
                            isjspceoalfofcoahodeoc,
                            principal_carer,
                            fortnightly_partner_income,
                            annual_partner_income,
                            age,
                            fy.year,
                            assets_value,
                            homeowner,
                            lower,
                            upper,
                            taper_lower,
                            taper_upper,
                            taper_principal_carer)

  input <-
    data.table(fortnightly_income = as.double(fortnightly_income),
               annual_income = as.double(annual_income),
               has_partner,
               partner_pensioner,
               n_dependants,
               nine_months,
               isjspceoalfofcoahodeoc,
               principal_carer,
               fortnightly_partner_income,
               annual_partner_income,
               age,
               fy.year,
               assets_value,
               homeowner,
               lower,
               upper,
               taper_lower,
               taper_upper,
               taper_principal_carer)
  
  if (input[, any(!has_partner & partner_pensioner)]) {
    stop('check conflicting values for `has_partner` and `partner_pensioner`')
  }
  
  if (input[, any(annual_partner_income > 0 & !has_partner)]) {
    stop('check conflicting values for `has_partner` and `annual_partner_income`')
  }
  
  if (input[, any(fortnightly_partner_income > 0 & !has_partner)]) {
    stop('check conflicting values for `has_partner` and `fortnightly_partner_income`')
  }
  
  #replace missing fortnightly income with annual income / 26
  input[, fortnightly_income := if_else(annual_income > 0 & fortnightly_income == 0,
                                        annual_income / 26,
                                        fortnightly_income)]
  input[, fortnightly_partner_income := if_else(annual_partner_income > 0 & fortnightly_partner_income == 0,
                                                annual_partner_income / 26,
                                                fortnightly_partner_income)]
  
  #replace missing annual income with fortnightly income * 26
  input[, annual_income := if_else(fortnightly_income > 0 & annual_income == 0,
                                   fortnightly_income * 26,
                                   annual_income)]
  input[, annual_partner_income := if_else(fortnightly_partner_income > 0 & annual_partner_income == 0,
                                           fortnightly_partner_income * 26,
                                           annual_partner_income)]
  
  if(!input[, isTRUE(all.equal(annual_income, 26 * fortnightly_income, tol = 1e-04))]){
    stop('input for `annual_income` is not 26 times larger than `fortnightly_income`')
  }
  
  if(!input[, isTRUE(all.equal(annual_partner_income, 26 * fortnightly_partner_income, tol = 1e-04))]){
    stop('input for `annual_partner_income` is not 26 times larger than `fortnightly_partner_income`')
  }

  max_rate_March_2016 <- NULL
  input[, max_rate_March_2016 := if_else(isjspceoalfofcoahodeoc,
                                         737.10,
                                         if_else(has_partner,
                                                 476.4,
                                                 if_else(and(age >= 60, nine_months),
                                                         570.80,
                                                         if_else(n_dependants > 0,
                                                                 570.80,
                                                                 527.60))))]
  
  # Additional eligibility requirements at https://www.humanservices.gov.au/individuals/services/centrelink/newstart-allowance/who-can-get-it
  eligible <- NULL
  input[, eligible := 22 <= age & age < 65]
  
  max_income_March_2016 <- NULL
  input[, max_income_March_2016 := if_else(isjspceoalfofcoahodeoc,
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

  # If partner is on pension, fortnightly income is average of 2 incomes
  input[(partner_pensioner),
        fortnightly_income := (fortnightly_partner_income + fortnightly_income) / 2]
  
  # Income reduction
  income_reduction <- NULL
  input %>%
    .[, income_reduction := 0] %>%
    .[fortnightly_income > lower,
      income_reduction := if_else(principal_carer,
                                  if_else(fortnightly_income < max_income_March_2016,
                                          taper_principal_carer * (fortnightly_income - lower),
                                          max_rate_March_2016),
                                  if_else(fortnightly_income < upper,
                                          taper_lower * (fortnightly_income - lower),
                                          if_else(fortnightly_income < max_income_March_2016,
                                                  taper_lower * (fortnightly_income - lower) +
                                                    (taper_upper - taper_lower) * (fortnightly_income - upper),
                                                  max_rate_March_2016)))]
  # Asset test
  asset_threshold <- NULL
  input[, asset_threshold := if_else(has_partner,
                                     if_else(homeowner,
                                             assets_value < 286500,
                                             assets_value < 433000),
                                     if_else(homeowner,
                                             assets_value < 202000,
                                             assets_value < 348500))]
  
  # Partner income reduction
  # https://web.archive.org/web/20160812171654/http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
  partner_income_reduction <- NULL
  input %>%
    .[, partner_income_reduction := 
        and(has_partner,
            (fortnightly_partner_income > max_income_March_2016) & !partner_pensioner) *
        taper_upper * 
        (fortnightly_partner_income -
           ceiling((max_rate_March_2016 -
                      (upper - lower) * taper_lower +
                      upper * taper_upper) / taper_upper))]
  
  # Check eligibility
  fortnightly_rate <- NULL
  input[, fortnightly_rate :=
          and(eligible, asset_threshold) *
          pmax0(max_rate_March_2016 - income_reduction - partner_income_reduction)]
        
  ans <- input[, fortnightly_rate * 26]
  ans / validate_per(per, missing(per))
  
}
