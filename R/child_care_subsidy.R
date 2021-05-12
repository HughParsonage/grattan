#' Child Care Subsidy paid per child. 
#' 
#' @param family_annual_income (numeric) Total income of the family.
#' @param activity_level (numeric) The total number of activity hours of the 
#' parent. Note that if there are two parents the one with the lower activity 
#' level will be applied. Common activities include work, leave, and study. A 
#' full list can be viewed at \url{https://guides.dss.gov.au/family-assistance-guide/3/5/2/10}.
#' @param activity_exemption (logical) Whether the parent is exempt from the 
#' activity test. Note that in a two parent family both parents must be exempt. 
#' A list of exemptions is available at \url{https://guides.dss.gov.au/family-assistance-guide/3/5/2/10}.
#' @param child_age (numeric) The age of the child in child care.
#' @param type_of_day_care (character) The type of child care. Acceptable inputs
#'  are: \code{"cbdc"} Centre Based Day Care, "oshc" Outside School Hours Care,
#'  \code{"fdc"} Family Day Care, or 
#'  \code{"ihc"} In Home Care. Note that In Home Care 
#'  can only be claimed once per family.
#' @param hours_day_care_fortnight (numeric) The number of hours of day care per
#'  child per fortnight.
#' @param cost_hour (numeric) The cost of day care per hour.
#' @param early_education_program (logical) Whether the child is part of an 
#' early education program.
#' 
#' @param cbdc_hourly_cap,fdc_hourly_cap,oshc_hourly_cap,ihc_hourly_cap (numeric) 
#' The lower of `cost_hour` or the relevant `hourly_cap` will be used in the 
#' calculation of the subsidy.
#' @param annual_cap_income (numeric) The minimum family income for which the 
#' `annual_cap_subsidy` applies from.
#' @param annual_cap_subsidy (numeric) Amount at which annual subsidies are 
#' capped for those who earn more than `annual_cap_income`. 
#' 
#' @param activity_test_1_brackets (numeric vector) The activity levels at which
#'  the activity test increases.
#' @param activity_test_1_hours (numeric vector) The hours corresponding to the
#'  step increase in `activity_test_1_brackets`.
#' 
#' @param income_test_bracket_1,income_test_bracket_2,income_test_bracket_3,income_test_bracket_4,income_test_bracket_5 (numeric) The steps at which income test 1 changes rates.
#'  Note the strange structure \code{https://www.servicesaustralia.gov.au/individuals/services/centrelink/child-care-subsidy/how-much-you-can-get/your-income-can-affect-it}.
#' @param taper_1,taper_2,taper_3 (numeric) The proportion of the hourly cap 
#' retained. Note that the rate only decreases between each odd bracket.
#' 
#' @return The annual child care subsidy payable per child.
#' 
#' @examples
#' child_care_subsidy(family_annual_income = 175000,
#'                    activity_level = 40,
#'                    activity_exemption = FALSE,
#'                    child_age = 3,
#'                    type_of_day_care = "cbdc",
#'                    cost_hour = 20,
#'                    hours_day_care_fortnight = 80,
#'                    early_education_program = FALSE)
#'                    
#'@export 



child_care_subsidy <- function(family_annual_income = 0,
                               activity_level = Inf,
                               activity_exemption = FALSE,
                               child_age = 3,
                               type_of_day_care = c("cbdc", "oshc", "fdc", "ihc"),
                               hours_day_care_fortnight = 36,
                               cost_hour = 10,
                               early_education_program = FALSE,
                               
                               cbdc_hourly_cap = 11.77,
                               fdc_hourly_cap = 10.90,
                               oshc_hourly_cap = 10.29,
                               ihc_hourly_cap = 25.48,
                               
                               annual_cap_income = 186958,
                               annual_cap_subsidy = 10190,
                               
                               income_test_bracket_1 = 66958,
                               income_test_bracket_2 = 171958,
                               income_test_bracket_3 = 251248,
                               income_test_bracket_4 = 341248,
                               income_test_bracket_5 = 354248,
                               taper_1 = 0.85,
                               taper_2 = 0.5,
                               taper_3 = 0.2,
                               
                               activity_test_1_brackets = c(0, 8, 16.00001, 48.00001),
                               activity_test_1_hours = c(0, 36, 72, 100)) {
  
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
  type_of_day_care <- match.arg(type_of_day_care, several.ok = TRUE)
  
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
  
  # Income_test
  income_test <- NULL
  .data[, income_test := if_else(family_annual_income < income_test_bracket_1,
                                 taper_1,
                                 if_else(family_annual_income < income_test_bracket_2,
                                         taper_1 - floor((family_annual_income - income_test_bracket_1)/3000)/100,
                                         if_else(family_annual_income < income_test_bracket_3,
                                                 taper_2,
                                                 if_else(family_annual_income < income_test_bracket_4,
                                                         taper_2 - floor((family_annual_income - income_test_bracket_3)/3000)/100,
                                                         if_else(family_annual_income < income_test_bracket_5,
                                                                 taper_3,
                                                                 0)))))]
  # Type of care adjustment
  .data[, type_of_day_care := if_else(child_age >= 6 & type_of_day_care == "cbdc",
                                      "oshc",
                                      if_else(child_age < 6 & type_of_day_care == "oshc",
                                              "cbdc", 
                                              type_of_day_care))]
  setindexv(.data, "type_of_day_care")
  # Hourly cap
  hourly_cap <- NULL
  .data[, hourly_cap := 0]
  .data[type_of_day_care == "cbdc", hourly_cap := cbdc_hourly_cap]
  .data[type_of_day_care == "fdc", hourly_cap := fdc_hourly_cap]
  .data[type_of_day_care == "oshc", hourly_cap := oshc_hourly_cap]
  .data[type_of_day_care == "ihc", hourly_cap := ihc_hourly_cap]
  
  annual_cap <- NULL
  .data[, annual_cap := if_else(family_annual_income > annual_cap_income,
                                annual_cap_subsidy,
                                Inf)]
  
  activity_test_1 <- NULL
  # Activity tests
  .data[, activity_test_1 := koffset(activity_level,
                                     activity_test_1_brackets,
                                     activity_test_1_hours,
                                     Yright = 100,
                                     Method = "constant")]
  
  activity_test_2 <- NULL
  activity_test_3 <- NULL
  activity_test_4 <- NULL
  activity_test <- NULL
  .data[, activity_test_2 := if_else(family_annual_income <= 66985 & activity_level < 8,
                                     24, 0)]
  .data[, activity_test_3:= if_else(early_education_program & type_of_day_care == "cbdc",
                                     36, 0)]
  .data[, activity_test_4 := if_else(activity_exemption, 100, 0)]
  
  .data[, activity_test := pmax.int(activity_test_1,
                                    activity_test_2,
                                    activity_test_3,
                                    activity_test_4)]
  
  
  # Calculation
  .data[, pminV(pminV(cost_hour, hourly_cap) * income_test * pminV(hours_day_care_fortnight, activity_test) * 365/14,
                annual_cap)]
  
}
