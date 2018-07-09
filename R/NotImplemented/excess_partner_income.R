

partner_income_free_area <- function(partner_receives_benefit,
                                     max_benefit = NULL,
                                     age) {
  out <- 0
  out[age < 22] <- which.min(youth_allowance(ordinary_income = 1:10e3,
                                             age = 21, 
                                             eligible_if_over22 = TRUE,
                                             isjspceoalfofcoahodeoc = TRUE, 
                                             is_single = TRUE) <= 0)
}

excess_partner_income <- function(partner_age, 
                                  partner_receives_benefit,
                                  partner_receives_dependant_YA,
                                  partner_receives_pension) {
  prohibit_unequal_length_vectors(partner_age, 
                                  partner_receives_benefit, 
                                  partner_receives_dependant_YA, 
                                  partner_receives_pension)
  out <- rep_len(0, length(partner_receives_benefit))
  nzero <- nor(partner_receives_dependant_YA, partner_receives_pension)
  out[nzero] <-
    partner_income_free_area(partner_receives_benefit[nzero],
                             partner_age[nzero])
  
}
