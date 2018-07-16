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
carer_payment <- function(dclad_eligible = TRUE,
                          adat_eligible = FALSE,
                          n_dependents = 0,
                          high_adat = FALSE,
                          living_at_home = FALSE,
                          carer_fortnightly_income = 0,
                          carer_annual_income = 0,
                          carer_asset_value = 0,
                          care_receiver_fortnightly_income = 0,
                          care_receiver_annual_income = 0,
                          care_receiver_asset_value = 0,
                          partner_income = 0,
                          partner_assets = 0,
                          children_income = 0,
                          children_assets = 0
                          ) {
  
  input <- data.table(do.call(cbind.data.frame, mget(ls())))
  
  #Rates, income test, and asset test same as age pension
  
  #http://guides.dss.gov.au/guide-social-security-law/4/2/5
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  
  rate <- 10
  
  care_receiver_eligible <- input[ ,(dclad_eligible | adat_eligible)]
  
  income <- input[ ,if_else(high_adat,
                            care_receiver_annual_income + partner_income, #high adat
                            if_else(dclad_eligible & living_at_home,
                                    care_receiver_annual_income, #child living away from home
                                    care_receiver_annual_income + partner_income + children_income))] #all other cases
  assets <- input[ ,if_else(dclad_eligible & !living_at_home,
                            care_receiver_asset_value, #child living away from home
                            care_receiver_asset_value + partner_assets + children_assets)] #all other cases
  
  income_elligible <- (income < 108828)
  assets_elligible <- (assets < 671250)
  
  #OUTPUT
  input[ ,if_else(care_receiver_eligible & income_elligible & assets_elligible,
                  rate,
                  0)]
}
