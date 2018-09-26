context("Model Child Crae Subsidy")

test_that("Values", {
  library(data.table)
  sample <- CJ(family_annual_income = seq(0, 400000, 50000),
               activity_level = c(0, 16, 20),
               activity_exemption = FALSE,
               child_age = c(3, 7),
               type_of_day_care = c("cbdc", "oshc", "fdc", "ihc"),
               hours_day_care_fortnight = c(0, 30, 50),
               cost_hour = c(10,11,12,30),
               early_education_program = FALSE)
  
  m1819_3 <- function(tcbdc_hourly_cap = 11.77,
                      tfdc_hourly_cap = 10.90,
                      toshc_hourly_cap = 10.29,
                      tihc_hourly_cap = 25.48,
                      
                      tannual_cap_income = 186958,
                      tannual_cap_subsidy = 10190,
                      
                      tincome_test_bracket_1 = 66958,
                      tincome_test_bracket_2 = 171958,
                      tincome_test_bracket_3 = 251248,
                      tincome_test_bracket_4 = 341248,
                      tincome_test_bracket_5 = 354248,
                      ttaper_1 = 0.85,
                      ttaper_2 = 0.5,
                      ttaper_3 = 0.2,
                      
                      tactivity_test_1_brackets = c(0, 8, 16.00001, 48.00001),
                      tactivity_test_1_hours = c(0, 36, 72, 100),
                      ret = "new_css") {
    css <- model_child_care_subsidy(sample_file,
                                    
                                    Cbdc_hourly_cap = tcbdc_hourly_cap,
                                    Fdc_hourly_cap = tfdc_hourly_cap,
                                    Oshc_hourly_cap = toshc_hourly_cap,
                                    Ihc_hourly_cap = tihc_hourly_cap,
                                    
                                    Annual_cap_income = tannual_cap_income,
                                    Annual_cap_subsidy = tannual_cap_subsidy,
                                    
                                    Income_test_bracket_1 = tincome_test_bracket_1,
                                    Income_test_bracket_2 = tincome_test_bracket_2,
                                    Income_test_bracket_3 = tincome_test_bracket_3,
                                    Income_test_bracket_4 = tincome_test_bracket_4,
                                    Income_test_bracket_5 = tincome_test_bracket_5,
                                    Taper_1 = ttaper_1,
                                    Taper_2 = ttaper_2,
                                    Taper_3 = ttaper_3,
                                    
                                    Activity_test_1_brackets = tactivity_test_1_brackets,
                                    Activity_test_1_hours = tactivity_test_1_hours,
                                    
                                    return. = "new_css")
    if (ret == "new_css") {
      sum(css) 
    } else {
      css
    }
  }
  
  expect_gt(m1819_3(tcbdc_hourly_cap = 10),
            m1819_3(tcbdc_hourly_cap = 9))
  
  expect_gt(m1819_3(tfdc_hourly_cap = 10),
            m1819_3(tfdc_hourly_cap = 9))
  
  expect_gt(m1819_3(Annual_cap_income = 10000, tannual_cap_subsidy = 1000),
            m1819_3(Annual_cap_income = 10000, tannual_cap_subsidy = 100))
  
  expect_gt(m1819_3(tincome_test_bracket_1 = 60000),
            m1819_3(tincome_test_bracket_1 = 50000))
  
  expect_gt(m1819_3(tincome_test_bracket_2 = 180000),
            m1819_3(tincome_test_bracket_2 = 170000))
  
  expect_gt(m1819_3(ttaper_1 = 0.75),
            m1819_3(ttaper_1 = 0.85))
  
  expect_true(is.data.table(m1819_3(ret = "sample_file")))
  expect_true(is.data.table(m1819_3(ret = "sample_file.int")))
  expect_true(is.integer(.subset2(m1819_3(ret = "sample_file.int"),
                                  "baseline_css")))
})

test_that("Errors", {
  sample <- CJ(family_annual_income = seq(0, 400000, 50000),
               activity_levels = c(0, 16, 20),
               activity_exemption = FALSE,
               child_age = c(3, 7),
               type_of_day_care = c("cbdc", "oshc", "fdc", "ihc"),
               hours_day_care_fortnight = c(0, 30, 50),
               cost_hour = c(10,11,12,30),
               early_education_program = FALSE)
  expect_error(model_child_care_subsidy(sample),
               regexp = "`sample_file` lacked the following required columns:\n\tactivity_level.\n")
  expect_error(model_child_care_subsidy(300),
               regexp = "`sample_file` was a numeric, but must be a data.frame.")
  sample <- CJ(family_annual_income = seq(0, 400000, 50000),
               activity_level = c(0, 16, 20),
               activity_exemption = FALSE,
               child_age = c(3, 7),
               type_of_day_care = c("cbdc", "oshc", "fdc", "ihc"),
               hours_day_care_fortnight = c(0, 30, 50),
               cost_hour = c(10,11,12,30),
               early_education_program = FALSE)
  expect_error(model_child_care_subsidy(sample, 
                                        Cbdc_hourly_cap = "10"),
               regexp = "`Cbdc_hourly_cap` was type character, but must be numeric.")
  expect_error(model_child_care_subsidy(sample, 
                                        Cbdc_hourly_cap = c(10,20)),
               regexp = "`Cbdc_hourly_cap` had length 2 but must be length-one.")
})
