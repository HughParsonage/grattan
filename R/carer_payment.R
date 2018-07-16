#' Carer Payment
#' 
#' 
#' 
#' @details
#' 
#' 
#' 
#' @export carer_payment
#' 
#' 
carer_payment <- function(carer_ordinary_income = 0, 
                          carer_annual_income = carer_ordinary_income * 26, 
                          has_partner = FALSE,
                          n_dependants = 0L,
                          carer_partner_fortnightly_income = 0,
                          carer_partner_annual_income = carer_partner_fortnightly_income * 26,
                          Date = NULL,
                          fy.year = NULL,
                          carer_assets_value = 0,
                          is_home_owner = FALSE,
                          illness_separated_couple = FALSE,
                          
                          dclad_eligible = TRUE,
                          adat_eligible = FALSE,
                          high_adat = FALSE,
                          living_at_home = FALSE,
                          
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
  
  input <- data.table(do.call(cbind.data.frame, mget(ls())))

  #Rates, income test, and asset test same as age pension

  rate <- age_pension(ordinary_income = carer_ordinary_income,
                      annual_income = carer_annual_income,
                      has_partner = has_partner,
                      n_dependants = n_dependants,
                      partner_fortnightly_income = carer_partner_fortnightly_income,
                      partner_annual_income = carer_partner_annual_income,
                      Date = Date,
                      fy.year = fy.year,
                      assets_value = carer_assets_value,
                      is_home_owner = is_home_owner,
                      illness_separated_couple = illness_separated_couple)


  #http://guides.dss.gov.au/guide-social-security-law/4/2/5
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf

  care_receiver_eligible <- input[ ,(dclad_eligible | adat_eligible)]

  income <- input[ ,if_else(high_adat,
                            care_receiver_annual_income + partner_annual_income, #high adat
                            if_else(dclad_eligible & living_at_home,
                                    care_receiver_annual_income, #child living away from home
                                    care_receiver_annual_income + partner_annual_income + children_annual_income))] #all other cases
  assets <- input[ ,if_else(dclad_eligible & !living_at_home,
                            care_receiver_asset_value, #child living away from home
                            care_receiver_asset_value + partner_asset_value + children_asset_value)] #all other cases

  income_elligible <- (income < 108828)
  assets_elligible <- (assets < 671250)

  #OUTPUT
  input[ ,if_else(care_receiver_eligible & income_elligible & assets_elligible,
                  rate,
                  0)]
  
 
}
