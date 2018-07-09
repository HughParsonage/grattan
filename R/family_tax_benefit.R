#' FAMILY TAX BENEFIT
#' @param child_age
#' @param child_in_secondary_school
#' @param child_in_care
#' @export
#' 

family_tax_benefit <- function(data, 
                               income_test_1_bound = 51027,
                               income_test_2_bound = 94316,
                               per = 'fortnight'){
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  #data has columns `HH_id`, `id`, `age`, `income`, `in_secondary_school`, `in_care`
  
  eligible <- data[ ,(age <= 15) | ((16 <= age) & (age <= 19) & in_secondary_school)]
  
  data[ ,max_rate_March_2016 := if_else(age < 13,
                   179.76,
                   if_else(age <= 15,
                           233.94,
                           if_else(age <= 19 & in_secondary_school,
                                   233.94,
                                   if_else(in_care,
                                           57.68,
                                           0))))]
  
  base_rate_March_2016 <-
    57.68
  
  family_data <- temp %>% group_by(HH_id) %>% summarise(fam_income = sum(income), max_family_rate = sum(max_rate_March_2016))
    
    
    
  #income reduction tests
  family_data[ ,income_test_1 := if_else(fam_income < income_test_1_bound,
                    0,
                    0.2 * (fam_income - income_test_1_bound))]
  family_data[ ,income_test_2 := if_else(fam_income < income_test_2_bound,
                    0,
                    0.3 * (fam_income - income_test_2_bound))]
  
  rate <- family_data[ ,pmaxC(max_family_rate - income_test_1, base_rate_March_2016 - income_test_2)]
  
  #SUPPLEMENT?
  
  
  #OUTPUT
  rate
}
