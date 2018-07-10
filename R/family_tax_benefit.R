#' FAMILY TAX BENEFIT
#' @param child_age
#' @param child_in_secondary_school
#' @param child_in_care
#' @export
#' 

family_tax_benefit <- function(data, 
                               income_test_1_bound = 51027,
                               income_test_2_bound = 94316,
                               income_test_bound_ftbB = 5402,
                               per = 'annual'
                               ){
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  #https://web.archive.org/web/20160420184949/http://guides.dss.gov.au/family-assistance-guide/3/1/1/20
  #historical rates: http://guides.dss.gov.au/family-assistance-guide/3/6/
  
  #data has columns `HH_id`, `id`, `age`, `income`, `in_secondary_school`, `in_care`, `single_parent`
  #note: `has_partner_and_is_parent` condition is to sift out ftb B families who are ineligible
  
  ftbA_eligible <- data[ ,(age <= 15) | ((16 <= age) & (age <= 19) & in_secondary_school)]
  
  #ftbA rate
  data[ ,ftbA_max_rate_March_2016 := if_else(age < 13,
                                             179.76,
                                             if_else(age <= 15,
                                                     233.94,
                                                     if_else(age <= 19 & in_secondary_school,
                                                             233.94,
                                                             if_else(in_care,
                                                                     57.68,
                                                                     0))))]
  data[ ,ftbA_base_rate_March_2016 := if_else(ftbA_eligible,
                                              57.68,
                                              0)]
  
  data[ ,ftbA_supplement_March_2016 := if_else(ftbA_eligible,
                                               726.35,
                                               0)]
            #note 1: conditional on meeting requirements e.g. immunisations, tax returns
            #note 2: can only be paid annually
  
  
  family_data <- data %>% group_by(HH_id) %>% summarise(fam_income = sum(income), 
                                                        ftbA_max_family_rate = sum(ftbA_max_rate_March_2016), 
                                                        ftbA_base_family_rate = sum(ftbA_base_rate_March_2016),
                                                        ftbA_supplement_family_rate = sum(ftbA_supplement_March_2016),
                                                        primary_income = max(income), 
                                                        secondary_income = sort(income, decreasing = TRUE)[2],
                                                        youngest_age = min(age),
                                                        youngest_in_secondary = in_secondary_school[age==min(age)],
                                                        single_parent = any(single_parent)) %>% data.table()
  
  #ftbB
  #cannot be paid during Paid Parental Leave period
  ftbB_rate_March_2016 <- family_data[ ,if_else(youngest_age < 5,
                                                 152.88,
                                                 if_else(youngest_age <= 13,
                                                         106.82,
                                                         if_else(youngest_age <= 15,
                                                                 106.82,
                                                                 if_else(youngest_age <= 19 & youngest_in_secondary & single_parent,
                                                                         106.82,
                                                                         0))))]

  ftbB_supplement_March_2016 <-354.05


  #income reduction test ftbA
  income_test_1 <- family_data[ ,if_else(fam_income < income_test_1_bound,
                    0,
                    0.2 * (fam_income - income_test_1_bound))]

  income_test_2 <- family_data[ ,if_else(fam_income < income_test_2_bound,
                    0,
                    0.3 * (fam_income - income_test_2_bound))]
            #note: before 2015 income_test_2_bound increased based upon number of ftb children http://guides.dss.gov.au/family-assistance-guide/3/6/1 note 2G

  #income test ftbB
  ftbB_eligible <- family_data[ ,primary_income < 100000]

  income_test_ftbB <- family_data[ ,if_else(primary_income < 100000,
                                             if_else(single_parent,
                                                     0,
                                                     if_else(secondary_income > income_test_bound_ftbB,
                                                             0.2 * (secondary_income - income_test_bound_ftbB),
                                                             0)),
                                             0)]



  output<- data.table(HH_id = family_data$HH_id,
                      ftbA_incl_supplement = family_data[ ,pmax(365/14 * ftbA_max_family_rate + ftbA_supplement_family_rate - income_test_1, 
                                                         365/14 * ftbA_base_family_rate + ftbA_supplement_family_rate - income_test_2,
                                                         0)],

                      ftbB_incl_supplement = family_data[ ,if_else(ftbB_eligible, 
                                                                   pmaxC(365/14 * ftbB_rate_March_2016 + ftbB_supplement_March_2016 - income_test_ftbB,
                                                                         0), 
                                                                   0)])
                      

  output
}
