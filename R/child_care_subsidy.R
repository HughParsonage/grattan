#' Child care Subsidy
#' 
#' @param family_annual_income
#' 
#' @return jdjd
#' 
#' @export 
#' 
#' @example 
#' 
#' djdjd
#' 

child_care_subsidy <- function(family_annual_income,
                               child_age,
                               type_of_day_care) {
  
  prohibit_vector_recycling(family_annual_income)
  
  .data <- 
    data.table(family_annual_income,
               child_age,
               type_of_day_care)
  
  income_reduction <- .data[, if_else(family_annual_income < 66958,
                                      0.85,
                                      if_else(family_annual_income < 171958,
                                              0.85 - floor((family_annual_income - 66958)/3000)/100,
                                              if_else(family_annual_income < 251248,
                                                      0.5,
                                                      if_else(family_annual_income < 341248,
                                                              0.5 - floor((family_annual_income - 251248)/3000)/100,
                                                              if_else(family_annual_income < 354248,
                                                                      0.2,
                                                                      0)))))] 
  #type of care adjustment
  .data[, type_of_day_care := if_else(child_age >= 6 & type_of_day_care == "cbdc",
                                    "oshc",
                                    if_else(child_age < 6 & type_of_day_care == "oshc",
                                            "cbdc", 
                                            type_of_day_care))]
    
  hourly_cap <- .data[, if_else(type_of_day_care == "cbdc",
                        11.77,
                        if_else(type_of_day_care == "fdc",
                                10.90,
                                if_else(type_of_day_care == "oshc",
                                        10.29,
                                        if_else(type_of_day_care == "ihc",
                                                25.48,
                                                0))))]
  
  #calculation
  output <- data.table(type_of_day_care, hourly_cap)
  output
}