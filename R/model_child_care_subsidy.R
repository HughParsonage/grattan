#' Model Child Care Subsidy
#' @description The child care subsidy if threshholds and rates are changed.
#' 
#' @export

model_child_care_subsidy <- function(sample_file = 1,
                                     
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
                                     activity_test_1_hours = c(0, 36, 72, 100),
  
                                     calc_baseline_subsidy = FALSE,
                                     return. = c("sample_file", "new_subsidy")) {
  return. <- match.arg(return.)
  
  income_test_params <- c(missing(income_test_bracket_1), 
                          missing(income_test_bracket_2), 
                          missing(income_test_bracket_3), 
                          missing(income_test_bracket_4), 
                          missing(income_test_bracket_5),
                          missing(taper_1),
                          missing(taper_2),
                          missing(taper_3))
  if(any(income_test_params) && !all(income_test_params)) {
    warning("Proceed with caution when not editing all parameters of the income test. The tapering may not line up logically.")
  }
  
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
  
  #Actual calculation
  
}
