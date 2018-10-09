#' Model Child Care Subsidy
#' @description The child care subsidy if thresholds and rates are changed.
#'  (See \code{\link{child_care_subsidy}}.)
#' @param sample_file A sample file having the same variables as the data.frame in the example.
#' @param Cbdc_hourly_cap,Fdc_hourly_cap,Oshc_hourly_cap,Ihc_hourly_cap (numeric) The lower of `cost_hour` or the relevant `hourly_cap` will be used in the calculation of the subsidy.
#' 
#' @param Annual_cap_income (numeric) The minimum family income for which the `Annual_cap_subsidy` applies from.
#' @param Annual_cap_subsidy (numeric) Amount at which annual subsidies are capped for those who earn more than `Annual_cap_income`. 
#' 
#' @param Income_test_bracket_1,Income_test_bracket_2,Income_test_bracket_3,Income_test_bracket_4,Income_test_bracket_5 (numeric) The steps at which income test 1 changes rates. Note the strange structure \url{https://www.humanservices.gov.au/individuals/services/centrelink/child-care-subsidy/payments/how-your-income-affects-it}.
#' @param Taper_1,Taper_2,Taper_3 (numeric) The proportion of the hourly cap retained. Note that the rate only decreases between each odd bracket.
#' 
#' @param Activity_test_1_brackets (numeric vector) The activity levels at which the activity test increases.
#' @param Activity_test_1_hours (numeric vector) The hours corresponding to the step increase in `activity_test_1_brackets`.
#' 
#' @param calc_baseline_ccs (logical, default: \code{TRUE}) Should the current child care subsidy be included as a column in the result?
#' @param return. What should the function return? One of \code{subsidy}, \code{sample_file}, or \code{sample_file.int}. 
#' If \code{subsidy}, the subsidy received under the settings; if \code{sample_file}, the \code{sample_file},
#' but with variables \code{subsidy} and possibly \code{new_subsidy}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_subsidy} is coerced to integer.



#' @export

model_child_care_subsidy <- function(sample_file,
                                     
                                     Cbdc_hourly_cap = NULL,
                                     Fdc_hourly_cap = NULL,
                                     Oshc_hourly_cap = NULL,
                                     Ihc_hourly_cap = NULL,
                                     
                                     Annual_cap_income = NULL,
                                     Annual_cap_subsidy = NULL,
                                     
                                     Income_test_bracket_1 = NULL,
                                     Income_test_bracket_2 = NULL,
                                     Income_test_bracket_3 = NULL,
                                     Income_test_bracket_4 = NULL,
                                     Income_test_bracket_5 = NULL,
                                     Taper_1 = NULL,
                                     Taper_2 = NULL,
                                     Taper_3 = NULL,
                                     
                                     Activity_test_1_brackets = NULL,
                                     Activity_test_1_hours = NULL,
  
                                     calc_baseline_ccs = TRUE,
                                     return. = c("sample_file", "new_ccs", "sample_file.int")) {
  return. <- match.arg(return.)
  
  # Check sample file has correct format
  if (!is.data.table(sample_file)) {
    if (!is.data.frame(sample_file)) {
      stop("`sample_file` was a ", class(sample_file)[1],
           ", but must be a data.frame.")
    }
    
    sample_file <- as.data.table(sample_file)
  } else {
    sample_file <- copy(sample_file)
  }
  
  cols_required <- 
    c("family_annual_income",
      "activity_level",
      "activity_exemption",
      "child_age",
      "type_of_day_care",
      "hours_day_care_fortnight",
      "cost_hour",
      "early_education_program")
  
  if (!all(cols_required %chin% names(sample_file))) {
    absent_cols <- setdiff(cols_required, names(sample_file))
    stop("`sample_file` lacked the following required columns:\n\t",
         paste0(absent_cols, collapse = "\n\t"), ".\n")
  }
  
  # check other params
  check_num1(Cbdc_hourly_cap)
  check_num1(Fdc_hourly_cap)
  check_num1(Oshc_hourly_cap)
  check_num1(Ihc_hourly_cap)
  check_num1(Annual_cap_income)
  check_num1(Annual_cap_subsidy)
  check_num1(Income_test_bracket_1)
  check_num1(Income_test_bracket_2)
  check_num1(Income_test_bracket_3)
  check_num1(Income_test_bracket_4)
  check_num1(Income_test_bracket_5)
  check_num1(Taper_1)
  check_num1(Taper_2)
  check_num1(Taper_3)
  
  if (!is.numeric(Activity_test_1_brackets)) {
    stop("`Activity_test_1_brackets` was type ", typeof(Activity_test_1_brackets),
         ", but must numeric.")
  }
  if (anyNA(Activity_test_1_brackets)) {
    stop("`Activity_test_1_brackets` had missing values. Impute or remove these values.")
  }
  
  if (!is.numeric(Activity_test_1_hours)) {
    stop("`Activity_test_1_hours` was type ", typeof(Activity_test_1_hours),
         ", but must numeric.")
  }
  if (anyNA(Activity_test_1_hours)) {
    stop("`Activity_test_1_hours` had missing values. Impute or remove these values.")
  }
  if (length(Activity_test_1_hours) != length(Activity_test_1_brackets)) {
    stop("`Activity_test_1_hours` and `Activity_test_1_brackets` have different ",
         "lengths. Ensure that the hours and brackets arguments are numeric ",
         "vectors with the same length.")
  }
  
  
  check_TF(calc_baseline_ccs)
  
  # Actual calculation
  
  Family_annual_income = sample_file[['family_annual_income']]
  Activity_level = sample_file[['activity_level']]
  Activity_exemption = sample_file[['activity_exemption']]
  Child_age = sample_file[['child_age']]
  Type_of_day_care = sample_file[['type_of_day_care']]
  Cost_hour = sample_file[['cost_hour']]
  Hours_day_care_fortnight = sample_file[['hours_day_care_fortnight']]
  Early_education_program = sample_file[['early_education_program']]
  
  if (calc_baseline_ccs && return. != "new_ccs") {
    baseline_ccs <- child_care_subsidy(family_annual_income = Family_annual_income,
                                       activity_level = Activity_level,
                                       activity_exemption = Activity_exemption,
                                       child_age = Child_age,
                                       type_of_day_care = Type_of_day_care,
                                       cost_hour = Cost_hour,
                                       hours_day_care_fortnight = Hours_day_care_fortnight,
                                       early_education_program = Early_education_program)
    
    set(sample_file,
        j = "baseline_ccs",
        value = switch(return.,
                       "sample_file" = baseline_ccs,
                       "sample_file.int" = as.integer(baseline_ccs)))
  }
  
  ccs <- child_care_subsidy(family_annual_income = Family_annual_income,
                            activity_level = Activity_level,
                            activity_exemption = Activity_exemption,
                            child_age = Child_age,
                            type_of_day_care = Type_of_day_care,
                            cost_hour = Cost_hour,
                            hours_day_care_fortnight = Hours_day_care_fortnight,
                            early_education_program = Early_education_program,
                            
                            cbdc_hourly_cap = Cbdc_hourly_cap,
                            fdc_hourly_cap = Fdc_hourly_cap,
                            oshc_hourly_cap = Oshc_hourly_cap,
                            ihc_hourly_cap = Ihc_hourly_cap,
                            
                            annual_cap_income = Annual_cap_income,
                            annual_cap_subsidy = Annual_cap_subsidy,
                            
                            income_test_bracket_1 = Income_test_bracket_1,
                            income_test_bracket_2 = Income_test_bracket_2,
                            income_test_bracket_3 = Income_test_bracket_3,
                            income_test_bracket_4 = Income_test_bracket_4,
                            income_test_bracket_5 = Income_test_bracket_5,
                            taper_1 = Taper_1,
                            taper_2 = Taper_2,
                            taper_3 = Taper_3,
                            
                            activity_test_1_brackets = Activity_test_1_brackets,
                            activity_test_1_hours = Activity_test_1_hours)
  
  switch(return.,
        "new_ccs" = ccs[],
        "sample_file" = set(sample_file, j = "new_ra", value = ccs)[],
        "sample_file.int" = set(sample_file, j = "new_ra", value = as.integer(ccs))[])
  
}
