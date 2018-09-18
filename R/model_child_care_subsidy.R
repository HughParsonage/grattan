#' Model Child Care Subsidy
#' @description The child care subsidy if threshholds and rates are changed.
#' @param sample_file A sample file having the same variables as the data.frame in the example.
#' @param cbdc_hourly_cap,fdc_hourly_cap,oshc_hourly_cap,ihc_hourly_cap (numeric) The lower of `cost_hour` or the relevant `hourly_cap` will be used in the calculation of the subsidy.
#' 
#' @param annual_cap_income (numeric) The minimum family income for which the `annual_cap_subsidy` applies from.
#' @param annual_cap_subsidy (numeric) Amount at which annual subsidies are capped for those who earn more than `annual_cap_income`. 
#' 
#' @param income_test_bracket_1,income_test_bracket_2,income_test_bracket_3,income_test_bracket_4,income_test_bracket_5 (numeric) The steps at which income test 1 changes rates. Note the strange structure \url{https://www.humanservices.gov.au/individuals/services/centrelink/child-care-subsidy/payments/how-your-income-affects-it}.
#' @param taper_1,taper_2,taper_3 (numeric) The proportion of the hourly cap retained. Note that the rate only decreases between each odd bracket.
#' 
#' @param activity_test_1_brackets (numeric vector) The activity levels at which the activity test increases.
#' @param activity_test_1_hours (numeric vector) The hours corresponding to the step increase in `activity_test_1_brackets`.
#' 
#' @param calc_baseline_subsidy (logical, default: \code{TRUE}) Should the current child care subsidy be included as a column in the result?
#' @param return. What should the function return? One of \code{subsidy}, \code{sample_file}, or \code{sample_file.int}. 
#' If \code{subsidy}, the subsidy received under the settings; if \code{sample_file}, the \code{sample_file},
#' but with variables \code{subsidy} and possibly \code{new_subsidy}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_subsidy} is coerced to integer.



#' @export

model_child_care_subsidy <- function(sample_file,
                                     
                                     cbdc_hourly_cap = NULL,
                                     fdc_hourly_cap = NULL,
                                     oshc_hourly_cap = NULL,
                                     ihc_hourly_cap = NULL,
                                     
                                     annual_cap_income = NULL,
                                     annual_cap_subsidy = NULL,
                                     
                                     income_test_bracket_1 = NULL,
                                     income_test_bracket_2 = NULL,
                                     income_test_bracket_3 = NULL,
                                     income_test_bracket_4 = NULL,
                                     income_test_bracket_5 = NULL,
                                     taper_1 = NULL,
                                     taper_2 = NULL,
                                     taper_3 = NULL,
                                     
                                     activity_test_1_brackets = NULL,
                                     activity_test_1_hours = NULL,
  
                                     calc_baseline_ccs = TRUE,
                                     return. = c("sample_file", "new_ccs")) {
  return. <- match.arg(return.)
  
  # Check sample file has correct format
  if(!is.data.table(sample_file)) {
    if (!is.data.frame(sample_file)) {
      stop("`sample_file` was a ", class(sample_file)[1],
           ", but must be a data.frame.")
    }
    
    sample_file <- as.data.table(sample_file)
  } else {
    sample_file <- copy(sample_file)
  }
  
  cols_required <- c("family_annual_income",  "activity_level", "activity_exemption", "child_age", "type_of_day_care", "hours_day_care_fortnight", "cost_hour", "early_education_program")
  
  if (!all(cols_required %chin% names(sample_file))) {
    absent_cols <- setdiff(cols_required, names(sample_file))
    stop("`sample_file` lacked the following required columns:\n\t",
         paste0(absent_cols, collapse = "\n\t"), ".\n")
  }
  
  #check other params
  params <- c(cbdc_hourly_cap, fdc_hourly_cap, oshc_hourly_cap, ihc_hourly_cap, 
              annual_cap_income, annual_cap_subsidy,
              income_test_bracket_1, income_test_bracket_2, income_test_bracket_3, income_test_bracket_4, income_test_bracket_5,
              taper_1, taper_2, taper_3,
              activity_test_1_brackets, activity_test_1_hours)
  
  for (i in params) {
    if (!is.null(i)) {
      check_numeric(i)
    }
  }
  
  #Actual calculation
  
  Family_annual_income = sample_file[['family_annual_income']]
  Activity_level = sample
  Activity_exemption = FALSE
  Child_age = 3
  Type_of_day_care = c("cbdc", "oshc", "fdc", "ihc")
  Hours_day_care_fortnight
  Cost_hour
  Early_education_program = FALSE
  
  if (calc_baseline_ccs && return. != "new_ccs") {
    baseline_ccs <- child_care_subsidy()
    
    set(sample_file,
        j = "baseline_ra",
        value = switch(return.,
                       "sample_file" = baseline_ra,
                       "sample_file.int" = as.integer(baseline_ra)))
  }
  
}
