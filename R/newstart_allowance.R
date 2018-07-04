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
newstart_allowance <- function(ordinary_income,
                               has_partner = FALSE,
                               n_dependants = 0L,
                               nine_months = FALSE,
                               isjspceoalfofcoahodeoc = FALSE,
                               principal_carer = FALSE,
                               partner_income = 0,
                               partner_eligible = FALSE,
                               age = NULL,
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
                    if_else(and(age > 60, nine_months),
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
    if_else(ordinary_income<lower,
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
  
  #output
  max_income_March_2016 - income_reduction - asset_reduction
            
  
}


.newstart_allowance_explicit <- function(ordinary_income,
                                         ordinary_income_free_area = 104,
                                         partner_income = 0,
                                         partner_receives_benefit = FALSE,
                                         partner_receives_pension = FALSE,
                                         partner_receives_dependent_YA = FALSE,
                                         partner_age = 22) {
  
  # If the recipient is a member of a couple, determine the partner income free area.
  if (NOR(partner_receives_benefit, partner_receives_pension)) {
    if (partner_age < 22) {
      
    }
  }
  
  # If the partner receives…	Personal income is…	Excess partner income is…
  # a social security benefit (1.1.S.190), other than dependent YA,	 	the amount by which the partner's income exceeds the partner income free area.
  # no payment,	their own income,	the amount by which the partner's income exceeds the partner income free area.
  # a social security pension (1.1.S.220) or a service pension (1.1.S.90),	half the couple's combined income,	zero.
  # dependent YA (1.1.D.100),	their own income,	zero.
  
  if (partner_receives_benefit && !partner_receives_dependent_YA) {
    personal_income <- ordinary_income
    excess_partner_income <- pmaxC(partner_income - partner_income_free_area, 0)
  }
  if (!partner_receives_benefit) {
    personal_income <- ordinary_income
    excess_partner_income <- pmaxC(partner_income - partner_income_free_area, 0)
  }
  if (partner_receives_pension) {
    personal_income <- (ordinary_income + partner_income) / 2
    excess_partner_income <- 0
  }
  if (partner_receives_benefit && partner_receives_dependent_YA) {
    personal_income <- ordinary_income
    excess_partner_income <- 0
  }
  
}

if (FALSE) {
# if (partner_re)

  
  

# 2	
  # Does the person's ordinary income exceed the ordinary income free area of $104.00 per fortnight (pf) for NSA, WA, PA, PPP, and SA, or $143.00 for YA job seeker, or $437.00 pf (2017 value) for full-time YA students (including Australian apprentices) and Austudy recipients?
  ordinary_income_excess <- 
    if_else(ordinary_income <= ordinary_income_free_area,
            0,
            ordinary_income - ordinary_income_free_area)
  #   If NO, the ordinary income excess is NIL.
  # If YES, the difference is THE ORDINARY INCOME EXCESS.
  
  
  

  
  # 3	
  # Determine the ordinary income reduction by using step 4A if the person is a full-time YA student (including Australian apprentices) or an Austudy recipient, or step 4B for a YA job seeker (including YA single principal carer parents), or step 4C for NSA single principal carer parents and 4D for other cases.
  
  # (The ordinary income reduction is the amount that will be deducted from the person's rate of payment in respect of their own income.)

    # 4C	
    # For any income of $104.00 and over, multiply by 0.4.
    # 
    # RESULT: THE ORDINARY INCOME REDUCTION.
    # Go to step 7.
    # 
    # 4D	
    # For any income between $104.00 and $254.00 pf, multiply by 0.5. The result is amount A.
    # 
    # For remaining income above $254.00 pf, multiply by 0.6. The result is amount B.
    # 
    # Add amount A and amount B.
    # 
    # RESULT: THE ORDINARY INCOME REDUCTION.
    # 5	
    # If the person is a member of a couple, determine the excess partner income
    # 
    # (see the tables under the previous headings for an explanation of this concept).
    # 
    # 6	
    # Multiply the excess partner income by 0.6.
    # 
    # RESULT: THE PARTNER INCOME REDUCTION.
    # 
    # 7	
    # Determine the person's income reduction by adding:
    #   
    #   the person's ordinary income reduction
    # the partner income reduction.
    # RESULT: THE PERSON'S INCOME REDUCTION.
# }
}

