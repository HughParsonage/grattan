#' Newstart allowance
#' 
#' @param ordinary_income 'Ordinary income' within the meaning of s. 1068-G1 of the \emph{Social Security Act 1991}. 
#' @param partner_income The partner's income.
#' @param age The individual's age. Different rates apply to those aged 18 to 20.
#' @param per Fortnight, the default, specified the units the inputs and outputs are returned.
#' @source \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/ssa1991186/s1068.html}
#' 


newstart_allowance <- function(ordinary_income,
                               has_partner = FALSE,
                               n_dependants = 0L,
                               nine_months = FALSE,
                               isjspceoalfofcoahodeoc = FALSE,
                               partner_income = 0,
                               partner_eligible = FALSE,
                               age = NULL,
                               Date = NULL,
                               income_free_area = NULL,
                               income_threshold = NULL,
                               Rate = NULL,
                               taper1 = 0.5,
                               taper2 = 0.6,
                               per = "fortnight") {
  if (is.null(Date)) {
    if (is.null(income_free_area) || is.null(income_threshold) || is.null(Rate)) {
      stop("`Date` is NULL, but `income_free_area`, ",
           "`income_threshold`, and `Rate` are not all provided.\n\n",
           "Supply:\n", 
           "\t`Date`\tor\t`income_free_area`,\n",
           "\t\t\t`income_threshold`, and\n", 
           "\t\t\t`Rate`.")
    } else {
      if_else(ordinary_income < income_free_area,
              Rate,
              pmaxC(Rate - if_else(ordinary_income < income_threshold, 
                                   taper1 * (ordinary_income - income_free_area),
                                   taper2 * (ordinary_income - income_threshold)),
                    0))
    }
  } else {
    input <- 
      data.table(Date = Date,
                 ordinary_income = ordinary_income) %>%
      .[, ordering := .I] %>%
      
      .[]
  }
  
  # Extract table
  
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

