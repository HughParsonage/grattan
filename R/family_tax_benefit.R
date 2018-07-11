#' FAMILY TAX BENEFIT
#' 
#' @param data Data table input. Each row is an individual. Column names are `HH_id`: household id, `id`: individual id, `age`: individual's age, `income`: individual's income, `in_secondary_school`: does the individual attend secondary school, `single_parent`: is the parent single, `other_allowance_benefit_or_pension`: does the individual receive a pension, benefit, or labour market program payment such as Youth Allowance, `maintenance_income`: the amount of maintenance income the individual receives for the care of a child/children from a previous relationship, `maintenance_children`: the number of children who you receive maintenance for that are in you care.
#' @param income_test_ftbA_1_bound Lower bound for which reduction in ftb A max payment occurs at rate taper_ftbA_1.
#' @param income_test_ftbA_2_bound Lower bound for which reduction in ftb A base payment occurs at rate taper_ftbA_1.
#' @param income_test_ftbB_bound Lower bound for which reduction in ftb B payment occurs at rate taper_ftbB.
#' @param taper_ftbA_1 The amount at which ftb A max payment is reduced for each dollar earned above income_test_ftbA_1_bound.
#' @param taper_ftbA_2 The amount at which ftb A base payment is reduced for each dollar earned above income_test_ftbA_2_bound.
#' @param taper_ftbB The amount at which ftb B payment is reduced for each dollar earned above income_test_ftbB_bound.
#' @param per How often the payment will be made. At present payments can only be annually.
#' @author Matthew Katzen
#' @export
#' 

family_tax_benefit <- function(data, 
                               income_test_ftbA_1_bound = 51027,
                               income_test_ftbA_2_bound = 94316,
                               income_test_ftbB_bound = 5402,
                               taper_ftbA_1 = 0.2,
                               taper_ftbA_2 = 0.3,
                               taper_ftbB = 0.2,
                               per = 'annual'
                               ){
  #https://www.humanservices.gov.au/sites/default/files/co029-1603en.pdf
  #https://web.archive.org/web/20160420184949/http://guides.dss.gov.au/family-assistance-guide/3/1/1/20
  #historical rates: http://guides.dss.gov.au/family-assistance-guide/3/6/
  
  
  #maintenance warning
  if(data[ ,any((maintenance_income > 0) == (maintenance_children == 0))]){
    stop("Incompatible combination of `maintenance_income` and `maintenance_children`")
  }
  
  #ftbA: payed per child
  data[ ,ftbA_max_rate_July_2015 := if_else(other_allowance_benefit_or_pension,
                                             0,
                                             if_else(age < 13,
                                                     179.76,
                                                     if_else(age <= 15,
                                                             233.94,
                                                             if_else(age <= 19 & in_secondary_school,
                                                                     233.94,
                                                                     0))))]
  data[ ,ftbA_base_rate_July_2015 := if_else(!other_allowance_benefit_or_pension & (age <= 15 | (age <= 19 & in_secondary_school)),
                                              57.68,
                                              0)]
  
  data[ ,ftbA_supplement_July_2015 := if_else(!other_allowance_benefit_or_pension & (age <= 15 | (age <= 19 & in_secondary_school)),
                                               726.35,
                                               0)]
            #note 1: supplement conditional on meeting requirements e.g. immunisations, tax returns
            #note 2: can only be paid annually
  
  #MAINTENANCE ACTION TEST: if you care for a child from a previous relationship and do not take reasonable action to attain child support, child is ineleigible for ftb A max rate (can still receive ftbA base payment).
  
  #MAINTENANCE INCOME TEST ftbA
  data[ ,maintenance_income_test_ftbA := if_else(maintenance_children > 0,
                                                 if_else(maintenance_children == 1,
                                                         pmax(0.5 * (maintenance_income - 1543.95), 0),
                                                         pmax(0.5 * (maintenance_income - 1543.95 - (514.65 * (maintenance_children - 1))), 0)),
                                                 0)] 
    
    
  
  family_data <- data %>% group_by(HH_id) %>% summarise(family_income = sum(income), 
                                                        ftbA_max_family_rate = sum(ftbA_max_rate_July_2015), 
                                                        ftbA_base_family_rate = sum(ftbA_base_rate_July_2015),
                                                        ftbA_supplement_family_rate = sum(ftbA_supplement_July_2015),
                                                        primary_income = max(income), 
                                                        secondary_income = if_else(any(single_parent), 
                                                                              0, 
                                                                              sort(income, decreasing = TRUE)[2]),
                                                        youngest_age = min(age),
                                                        youngest_in_secondary = in_secondary_school[age==min(age)],
                                                        youngest_allowance_benefit_or_pension = other_allowance_benefit_or_pension[age==min(age)],
                                                        single_parent = any(single_parent),
                                                        family_maintenance_income_test_ftbA = sum(maintenance_income_test_ftbA)) %>% data.table()
  
  #ftbB: payed based on youngest child
      #cannot be paid during Paid Parental Leave period
      #note: eligibility changed for fy 2016-17: https://www.humanservices.gov.au/sites/default/files/co029-1607en.pdf
  ftbB_rate_July_2015 <- family_data[ ,if_else(youngest_allowance_benefit_or_pension,
                                                0,
                                                if_else(youngest_age < 5,
                                                       152.88,
                                                       if_else(youngest_age <= 15 | (youngest_age <= 19 & youngest_in_secondary),
                                                                       106.82,
                                                                       0)))]
  
  ftbB_supplement_July_2015 <- family_data[ ,if_else(!youngest_allowance_benefit_or_pension & 
                                                      (youngest_age <= 15 | (youngest_age <= 19 & youngest_in_secondary)),
                                                      354.05,
                                                      0)]


  #income reduction test ftbA
  income_test_ftbA_1 <- family_data[ ,if_else(family_income < income_test_ftbA_1_bound,
                    0,
                    taper_ftbA_1 * (family_income - income_test_ftbA_1_bound))]

  income_test_ftbA_2 <- family_data[ ,if_else(family_income < income_test_ftbA_2_bound,
                    0,
                    taper_ftbA_2 * (family_income - income_test_ftbA_2_bound))]
            #note: before 2015 income_test_ftbA_2_bound increased based upon number of ftb children http://guides.dss.gov.au/family-assistance-guide/3/6/1 note 2G

  #income test ftbB
  ftbB_eligible <- family_data[ ,primary_income < 100000]

  income_test_ftbB <- family_data[ ,if_else(primary_income < 100000,
                                             if_else(single_parent,
                                                     0,
                                                     if_else(secondary_income > income_test_ftbB_bound,
                                                             taper_ftbB * (secondary_income - income_test_ftbB_bound),
                                                             0)),
                                             0)]

    output<- data.table(HH_id = family_data$HH_id,
                      ftbA_incl_supplement = family_data[ ,pmax(365/14 * ftbA_max_family_rate + ftbA_supplement_family_rate - income_test_ftbA_1 - family_maintenance_income_test_ftbA, 
                                                         365/14 * ftbA_base_family_rate + ftbA_supplement_family_rate - income_test_ftbA_2,
                                                         0)],

                      ftbB_incl_supplement = family_data[ ,if_else(ftbB_eligible, 
                                                                   pmaxC(365/14 * ftbB_rate_July_2015 + ftbB_supplement_July_2015 - income_test_ftbB,
                                                                         0), 
                                                                   0)])
                      

  output
}
