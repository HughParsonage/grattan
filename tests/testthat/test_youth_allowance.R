context("Youth allowance")

test_that("Error handling", {
  expect_error(youth_allowance(fortnightly_income = 1,
                               annual_income = 1),
               regexp = "`fortnightly_income` and `annual_income` both provided but don't agree.")
  expect_error(youth_allowance(fortnightly_income = (0:1) * 200,
                               fy.year = yr2fy(2016:2018)),
               regexp = "`fy.year` had length 3, yet `fortnightly_income` had length 2.",
               fixed = TRUE)
  expect_error(youth_allowance(is_student = as.logical(0:7),
                               fy.year = yr2fy(2016:2018)),
               regexp = "`fy.year` had length 3, yet `is_student` had length 8.",
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               max_rate = 500),
               regexp = "`fy.year` has length 3 but `max_rate` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               taper1 = 0.4),
               regexp = "`fy.year` has length 3 but `taper1` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               taper2 = 0.4),
               regexp = "`fy.year` has length 3 but `taper2` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               FT_YA_student_lower = 0.4),
               regexp = "`fy.year` has length 3 but `FT_YA_student_lower` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               FT_YA_student_upper = 0.4),
               regexp = "`fy.year` has length 3 but `FT_YA_student_upper` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               FT_YA_jobseeker_lower = 0.4),
               regexp = "`fy.year` has length 3 but `FT_YA_jobseeker_lower` is not NULL", 
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               FT_YA_jobseeker_upper = 0.4),
               regexp = "`fy.year` has length 3 but `FT_YA_jobseeker_upper` is not NULL", 
               fixed = TRUE)
  
})

test_that("Fortnightly annual agreement", {
  expect_equal(youth_allowance(annual_income = 26 * 260,
                               fy.year = "2015-16",
                               per = "year"),
               youth_allowance(fortnightly_income = 260,
                               fy.year = "2015-16",
                               per = "year"))
})

test_that("fy.year NULL", {
  expect_message(youth_allowance(per = "year"),
                 regex = '`fy.year` not set, so using `fy.year = ',
                 fixed = TRUE)
})

test_that("per", {
  expect_equal(youth_allowance(fy.year = "2016-17", 
                               per = "year"),
               youth_allowance(fy.year = "2016-17",
                               per = "fortnight") * 26)
  expect_equal(youth_allowance(fortnightly_income = 1:500,
                               fy.year = "2016-17", 
                               per = "year"),
               youth_allowance(fortnightly_income = 1:500,
                               fy.year = "2016-17",
                               per = "fortnight") * 26)
})

test_that("Youth allowance values for given financial years", {
  
})

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/40
test_that("Allowance with no income", {
  skip("Not yet implemented")
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 16,
                               has_partner = FALSE,
                               living_at_home = TRUE),
               244.10)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 18,
                               has_partner = FALSE,
                               living_at_home = TRUE),
               293.60)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 17,
                               has_partner = FALSE,
                               n_dependants = 0L,
                               living_at_home = FALSE),
               445.80)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 16,
                               has_partner = FALSE,
                               n_dependants = 1L),
               584.20)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 20,
                               isjspceoalfofcoahodeoc = TRUE),
               762.40)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 20,
                               has_partner = TRUE,
                               n_dependants = 0L),
               445.80)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 28,
                               has_partner = TRUE, 
                               n_dependants = 1L),
               489.60)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 22,
                               eligible_if_over22 = TRUE,
                               living_at_home = TRUE),
               360.20)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 25,
                               eligible_if_over22 = TRUE,
                               living_at_home = FALSE),
               541.70)
  expect_equal(youth_allowance(fortnightly_income = 0,
                               age = 25,
                               eligible_if_over22 = TRUE,
                               has_partner = TRUE),
               489.60)
})

test_that("Income reduction", {
  skip("Not yet implemented")
  #max income formula: https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
  #Student
  expect_equal(ya_income_reduction(fortnightly_income = 300,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0)
  expect_equal(ya_income_reduction(fortnightly_income = 500,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (500 - 437))
  expect_equal(ya_income_reduction(fortnightly_income = 700,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (700 - 524))
  expect_equal(ya_income_reduction(fortnightly_income = 860,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (860 - 524))
  
  #Jobseeker
  expect_equal(ya_income_reduction(fortnightly_income = 100,
                                max_rate_March_2018 = 244.10,
                                max_income = 574.18,
                                is_student = FALSE),
               0)
  expect_equal(ya_income_reduction(fortnightly_income = 200,
                                max_rate_March_2018 = 244.10,
                                max_income = 574.18,
                                is_student = FALSE),
               0.5 * (200 - 143))
  expect_equal(ya_income_reduction(fortnightly_income = 300,
                                max_rate_March_2018 = 244.10,
                                max_income = 574.18,
                                is_student = FALSE),
               0.5 * (250 - 143) + 0.6 * (300 - 250))
  
})

#http://guides.dss.gov.au/guide-social-security-law/5/5/2
#test_that('Allowance with income reduction',{
#  expect_equal()
#})
 
test_that('Income reduction not exceed max rate', {
  skip('threshold on https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411 contradicts taper rates')
  #When entering income values near to the maximum income threshold, in some cases payment reduction exceeds the maximum rate. 
  #One would expect payment reduction to approach the max rate as income approaches the max income. 
  expect_lt(ya_income_reduction(fortnightly_income = 860,
                             max_rate_March_2018 = 244.10,
                             max_income = 864.84,
                             is_student = TRUE),
            244.10)
  expect_equal(ya_income_reduction(fortnightly_income = 864.84,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (864.84 - 524))
  expect_equal(ya_income_reduction(fortnightly_income = 864.84,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (864.84 - 524))
})



