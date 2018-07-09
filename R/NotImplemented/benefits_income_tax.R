

benefits_income_test <- function(ordinary_income, 
                                 benefit = c("NSA", "NSA-single-principal-carer",
                                             "WA", "PA", "PPP", "SA",
                                             "YA-job-seeker", "YA-full-time"),
                                 partner_has_benefit = FALSE,
                                 partner_benefit = NULL,
                                 partner_age = NULL,
                                 has_partner = FALSE) {
  ordinary_income_free_area <-
    104 + 39 * (benefit == "YA-job-seeker") + (437 - 104) * (benefit == "YA-full-time")
  
  ordinary_income_reduction <- 
    if_else(benefit == "YA-full-time", 
            # 4A
            pmaxC(ordinary_income - 437, 0) * 0.5 + pmaxC(ordinary_income - 524, 0) * 0.1,
            
            if_else(benefit == "YA-job-seeker",
                    pmaxC(ordinary_income - 143, 0) * 0.5 + pmaxC(ordinary_income - 250, 0) * 0.1,
                    if_else(benefit == "NSA-single-principal-carer",
                            # 4C
                            pmaxC(ordinary_income - 104, 0) * 0.4,
                            # 4D
                            pmaxC(ordinary_income - 104, 0) * 0.5 + pmaxC(ordinary_income - 254, 0) * 0.1)))
  
  min_income_zero_benefit <- function(benefit) {
    
  }
  
  excess_partner_income <-
    has_partner * if_else(partner_has_benefit,
                          min_income_zero_benefit(),
                          if_else(partner_age))
  
  ordinary_income_reduction + excess_partner_income * 0.6
}
