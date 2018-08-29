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

child_care_subsidy <- function(family_annual_income = 0,
                               activity_level = Inf,
                               activity_exemption = TRUE,
                               child_age = 3,
                               type_of_day_care = c("cbdc", "oshc", "fdc", "ihc"),
                               hours_day_care_fortnight = 20,
                               cost_hour = 20,
                               early_education_program = FALSE) {
  
  prohibit_vector_recycling(family_annual_income,
                            activity_level,
                            activity_exemption,
                            child_age,
                            type_of_day_care,
                            hours_day_care_fortnight,
                            cost_hour,
                            early_education_program)
  
  arguments <- ls()
  argument_vals <- as.list(environment())
  type_of_day_care <- match.arg(type_of_day_care)
  
  if (!is.numeric(family_annual_income)) {
    stop("`family_annual_income` was type ", typeof(family_annual_income),", but must be numeric.")
  }
  if (!is.numeric(activity_level)) {
    stop("`activity_level` was type ", typeof(activity_level),", but must be numeric.")
  }
  if (!is.logical(activity_exemption)) {
    stop("`activity_exemption` was type ", typeof(activity_exemption),", but must be logical")
  }
  if (!is.numeric(child_age)) {
    stop("`child_age` was type ", typeof(child_age),", but must be numeric.")
  }
  if (!is.numeric(hours_day_care_fortnight)) {
    stop("`hours_day_care_fortnight` was type ", typeof(hours_day_care_fortnight),", but must be numeric.")
  }
  if (!is.numeric(cost_hour)) {
    stop("`cost_hour` was type ", typeof(cost_hour),", but must be numeric.")
  }
  if (!is.logical(early_education_program)) {
    stop("`early_education_program` was type ", typeof(early_education_program),", but must be logical")
  }
  
  .data <- 
    data.table(family_annual_income,
               activity_level,
               activity_exemption,
               child_age,
               type_of_day_care,
               hours_day_care_fortnight,
               cost_hour,
               early_education_program)
  
  income_test <- .data[, if_else(family_annual_income < 66958,
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
  
  annual_cap <- .data[, if_else(family_annual_income > 186958,
                                10190,
                                Inf)]
  
  activity_test_1 <- .data[, if_else(activity_level < 8,
                                     0,
                                     if_else(activity_level <= 16,
                                             36,
                                             if_else(activity_level <=  48,
                                                     72,
                                                     100)))]
  activity_test_2 <- .data[, if_else(family_annual_income <= 66985 & activity_level < 8,
                                     24, 0)]
  activity_test_3 <- .data[, if_else(early_education_program & type_of_day_care == "cbdc",
                                     36, 0)]
  activity_test_4 <- .data[, if_else(activity_exemption, 100, 0)]
  
  activity_test <- pmax(activity_test_1, activity_test_2, activity_test_3, activity_test_4)
  
  input <- data.table(cost_hour,
                      hourly_cap,
                      income_test,
                      hours_day_care_fortnight,
                      activity_test,
                      annual_cap)
  #calculation
  output <- input[, pminC(pminC(cost_hour, hourly_cap) * income_test * pminC(hours_day_care_fortnight, activity_test) * 365/14,
                          annual_cap)]
  output
}
