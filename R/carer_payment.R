#' Carer Payment
#' 
#' Carer payment is available to those who provide constant for a person who has a physical, intellectual, or psychiatric disability. Note that many of the arguments relate to the individual who receives the care (indicated by not starting with 'carer_'). Payment is made to the carer and not to the person receiving the care.
#' 
#' @param carer_fortnightly_income,carer_annual_income Carer's income for means-testing purposes. Provide one but not both.
#' @param carer_has_partner (logical, default: \code{FALSE}) Does the carer have a partner?
#' @param carer_n_dependants How many dependants does the carer have? Default is zero.
#' @param carer_partner_fortnightly_income,carer_partner_annual_income The carer's partner's income.
#' @param Date,fy.year The financial year. Currently only 2015-16 is supported (the most recent survey of income and housing results).
#' @param carer_assets_value Total value of carer's household assets.
#' @param carer_is_home_owner (logical, default: \code{FALSE}) Does the carer own their own home? 
#' @param carer_illness_separated_couple Is the couple separated by illness? (Affects the assets test.)
#' @param dclad_eligible Is the person receiving care a DCLAD qualifying child as defined in http://guides.dss.gov.au/guide-social-security-law/1/1/q/17 ?
#' @param low_adat,high_adat Does the person receiving care have a low or high ADAT score as defined in http://guides.dss.gov.au/guide-social-security-law/1/1/a/78 ?
#' @param living_at_home Does the person receiving care live at home with their parents?
#' @param care_receiver_fortnightly_income Care receiver's fortnightly income
#' @param care_receiver_asset_value Care receiver's asset value
#' @param partner_annual_income Care receiver's partner's annual income
#' @param partner_asset_value Care receiver's partner's asset value
#' @param children_annual_income Care receiver's partner's annual income
#' @param children_asset_value Care receiver's children's asset value
#' @param receiving_other_payment Is the care receiver receiving other social security payments?
#' 
#' @author Matthew Katzen
#' @export carer_payment
#' 
#' 
carer_payment <- function(Date = NULL,
                          fy.year = NULL,
                          carer_fortnightly_income = 0, 
                          carer_annual_income = carer_fortnightly_income * 26, 
                          carer_has_partner = FALSE,
                          carer_n_dependants = 0L,
                          carer_partner_fortnightly_income = 0,
                          carer_partner_annual_income = carer_partner_fortnightly_income * 26,
                          carer_assets_value = 0,
                          carer_is_home_owner = FALSE,
                          carer_illness_separated_couple = FALSE,
                          #care receiver arguments
                          dclad_eligible = TRUE,
                          low_adat = FALSE,
                          high_adat = FALSE,
                          living_at_home = FALSE,
                          receiving_other_payment = FALSE,
                          care_receiver_fortnightly_income = 0,
                          care_receiver_annual_income = 0,
                          care_receiver_asset_value = 0,
                          partner_annual_income = 0,
                          partner_asset_value = 0,
                          children_annual_income = 0,
                          children_asset_value = 0
                          ) {
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      Date <- .age_pension_today2qtr(Sys.Date())
      fy.year <- date2fy(Date)
      message('`Date` and `fy.year` not set, so using `Date = "', Date, '".')
    } else {
      Date <- fy2date(fy.year)
    }
  } else {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Date)
    } else {
      warning("`fy.year` and `Date` both used. Ignoring `fy.year`.")
    }
  }
  
  #error for no dclad or adat
  
  input <- data.table(do.call(cbind.data.frame, mget(ls())))

  #Rates, income test, and asset test same as age pension

  rate <- age_pension(ordinary_income = carer_fortnightly_income,
                      annual_income = carer_annual_income,
                      has_partner = carer_has_partner,
                      n_dependants = carer_n_dependants,
                      partner_fortnightly_income = carer_partner_fortnightly_income,
                      partner_annual_income = carer_partner_annual_income,
                      Date = Date,
                      assets_value = carer_assets_value,
                      is_home_owner = carer_is_home_owner,
                      illness_separated_couple = carer_illness_separated_couple)

  

  #http://guides.dss.gov.au/guide-social-security-law/4/2/5
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  
  #detailed eligibility: http://guides.dss.gov.au/guide-social-security-law/3/6/4
  care_receiver_eligible <- input[ ,(dclad_eligible | high_adat | low_adat)]

  care_receiver_income_test <- input[ ,if_else(high_adat,
                                               care_receiver_annual_income + partner_annual_income, #high adat
                                               if_else(dclad_eligible & living_at_home,
                                                       care_receiver_annual_income, #child living away from home
                                                       care_receiver_annual_income + partner_annual_income + children_annual_income))] #all other cases
  
  care_receiver_asset_test <- input[ ,if_else(dclad_eligible & !living_at_home,
                            care_receiver_asset_value, #child living away from home
                            care_receiver_asset_value + partner_asset_value + children_asset_value)] #all other cases

  income_eligible <- input[ ,if_else(high_adat & receiving_other_payment, #exemption: http://guides.dss.gov.au/guide-social-security-law/3/6/4/10
                                      TRUE,
                                      (care_receiver_income_test < 108828))]
  assets_eligible <- input[ ,if_else(high_adat & receiving_other_payment,
                                      TRUE,
                                      (care_receiver_asset_test < 671250))]

  #OUTPUT
  input[ ,if_else(care_receiver_eligible & income_eligible & assets_eligible,
                  rate,
                  0)]
}
