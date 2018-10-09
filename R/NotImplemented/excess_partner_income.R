

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

.partner_income_free_area <- function(fy.year,
                                      partner_receives_benefit,
                                      age) {
  input <- CJ(fy_year = fy.year,
              PartnerReceivesBenefit = partner_receives_benefit,
              Age = age)
  input[, II := .I]
  input[
    ,
    partner_income_free_area := if (length(Age) != 1L || length(fy_year) != 1L) {
      stop("Unexpected number of ages.")
    } else if (Age < 22) {
      which.max(youth_allowance(1:5e3, fy.year = fy_year, is_student = FALSE, per = "fortnight") < 0.01) + 1
    } else if (Age < 65) {
      which.max(unemployment_benefit(1:5e3, fy.year = fy_year) < 0.01) + 1
    } else {
      which.max(unemployment_benefit(1:5e3, fy.year = fy_year) < 0.01) + 1
    },
    keyby = "II"]
  input[, II := NULL]
  setkeyv(input, c("fy_year", "PartnerReceivesBenefit", "Age"))
  input[]
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
