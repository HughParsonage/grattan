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
  
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               age = c(16L, NA, 18L),
                               per = "fortnight"),
               regexp = "`age` contains missing values. Impute these values.",
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               age = 16L,
                               partner_is_pensioner = NA,
                               per = "fortnight"),
               regexp = "`partner_is_pensioner` contains missing values. Impute these values.",
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = yr2fy(2016:2018),
                               age = 16L,
                               partner_fortnightly_income = "abc",
                               per = "fortnight"),
               regexp = "`partner_fortnightly_income` was class character",
               fixed = TRUE)
  expect_error(youth_allowance(fy.year = "2017-18",
                               has_partner = c(FALSE, TRUE, FALSE),
                               partner_fortnightly_income = c(0, 0, 100)),
               regexp = "`partner_fortnightly_income` was greater than zero at position 3 yet `has_partner[3]` is FALSE.",
               fixed = TRUE)
  
})

test_that("Fortnightly annual agreement", {
  expect_equal(youth_allowance(annual_income = 26 * 260,
                               fy.year = "2015-16",
                               per = "year"),
               youth_allowance(fortnightly_income = 260,
                               fy.year = "2015-16",
                               per = "year"))
  
  expect_equal(youth_allowance(fortnightly_income = c(150, 260),
                               annual_income = 26 * c(150, 260),
                               fy.year = "2015-16",
                               per = "year"),
               youth_allowance(annual_income = 26 * c(150, 260),
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

test_that("Youth allowance values with and without energy supplemeent", {
  expect_equal(youth_allowance(fy.year = "2017-18", per = "fortnight"), 
               448.65)
  expect_equal(youth_allowance(fy.year = "2017-18", per = "fortnight", include_ES = FALSE),
               441.65)
})

test_that("Manually set youth allowance parameters coincide", {
  
  expect_equal(youth_allowance(fy.year = "2014-15", per = "fortnight"),
               youth_allowance(fy.year = "2014-15", per = "fortnight", 
                               max_rate = 427.6, include_ES = FALSE))
  
  # La plus ca meme
  ya <- function(...) {
    youth_allowance(fortnightly_income = 1:500,
                    fy.year = "2014-15", per = "fortnight", ...)
  }
  
  expect_equal(ya(),
               ya(max_rate = 427.6, include_ES = FALSE))
  expect_equal(ya(),
               ya(taper1 = 0.5))
  expect_equal(ya(),
               ya(taper2 = 0.6))
  expect_equal(ya(), 
               ya(FT_YA_student_lower = 421))
  expect_equal(ya(), 
               ya(FT_YA_student_upper = 505))
  
  # La plus ca meme
  ya <- function(...) {
    youth_allowance(fortnightly_income = 1:500,
                    is_student = FALSE,
                    fy.year = "2014-15", per = "fortnight", ...)
  }
  
  # No effect
  expect_equal(ya(), 
               ya(FT_YA_student_lower = 421))
  expect_equal(ya(), 
               ya(FT_YA_student_upper = 505))
  
  expect_equal(ya(), 
               ya(FT_YA_jobseeker_lower = 143))
  expect_equal(ya(), 
               ya(FT_YA_jobseeker_upper = 250))
  
  rm(ya)
})

test_that("Manually set different rates", {
  expect_lt(youth_allowance(fy.year = "2015-16", per = "year"),
            youth_allowance(fy.year = "2015-16", per = "year", max_rate = 500))
  expect_lt(youth_allowance(fortnightly_income = 550,
                            fy.year = "2015-16",
                            per = "year"),
            youth_allowance(fortnightly_income = 550,
                            fy.year = "2015-16",
                            per = "year",
                            taper2 = 0.4))
  expect_lt(youth_allowance(fortnightly_income = 500,
                            fy.year = "2015-16",
                            per = "year"),
            youth_allowance(fortnightly_income = 500,
                            fy.year = "2015-16",
                            per = "year",
                            taper1 = 0.4))
  expect_equal(youth_allowance(fortnightly_income = 520,
                               fy.year = "2015-16",
                               per = "fortnight",
                               taper2 = 0.4),
               youth_allowance(fortnightly_income = 520,
                               fy.year = "2015-16",
                               per = "fortnight") + 1)
  expect_equal(youth_allowance(fortnightly_income = c(520, 255),
                               is_student = c(TRUE, FALSE),
                               fy.year = "2015-16",
                               per = "fortnight",
                               taper2 = 0.4),
               youth_allowance(fortnightly_income = c(520, 255),
                               is_student = c(TRUE, FALSE),
                               fy.year = "2015-16",
                               per = "fortnight") + 1)
  expect_equal(youth_allowance(fortnightly_income = c(153, 439),
                               is_student = c(FALSE, TRUE),
                               fy.year = "2015-16",
                               per = "fortnight",
                               FT_YA_student_lower = 439,
                               FT_YA_jobseeker_lower = 153),
               youth_allowance(fortnightly_income = c(143, 429),
                               is_student = c(FALSE, TRUE),
                               fy.year = "2015-16",
                               
                               per = "fortnight"))
  expect_equal(youth_allowance(fortnightly_income = c(300, 600),
                               is_student = c(FALSE, TRUE),
                               has_partner = FALSE,
                               n_dependants = 0L,
                               fy.year = "2015-16",
                               per = "fortnight",
                               
                               FT_YA_jobseeker_upper = 200,
                               FT_YA_student_upper = 500,
                               taper1 = 0,
                               taper2 = 0.1),
               youth_allowance(fortnightly_income = 0,
                               is_student = c(FALSE, TRUE),
                               has_partner = FALSE,
                               n_dependants = 0L,
                               fy.year = "2015-16",
                               per = "fortnight") - 10)
})

test_that("Partners (standard)", {
  expect_equal(youth_allowance(fy.year = "2018-19",
                               age = 19L,
                               has_partner = TRUE,
                               per = "fortnight"),
               457.25)
  expect_equal(youth_allowance(fy.year = "2018-19",
                               age = 19L,
                               has_partner = TRUE,
                               n_dependants = 1L,
                               per = "fortnight"),
               502.2)
  
  ya_near_thr <- youth_allowance(fortnightly_income = 1280:1300,
                             fy.year = "2018-19",
                             age = 19L,
                             has_partner = TRUE,
                             n_dependants = 1L)
  expect_true(max(ya_near_thr) > 0)
  expect_equal(min(ya_near_thr), 0)
  
  expect_equal(length(youth_allowance(100,
                                      partner_fortnightly_income = 1000,
                                      has_partner = TRUE,
                                      partner_is_pensioner = TRUE)),
               1L)
  expect_equal(length(youth_allowance(100,
                                      age = c(18, 17, 22, 24, 66, 64),
                                      partner_fortnightly_income = 1000,
                                      has_partner = TRUE,
                                      partner_is_pensioner = TRUE)),
               6L)
  expect_equal(youth_allowance(100,
                               age = c(18, 17, 22, 24, 66, 64),
                               partner_fortnightly_income = 1000,
                               has_partner = TRUE,
                               partner_is_pensioner = TRUE)[2],
               youth_allowance(100,
                               age = 17,
                               partner_fortnightly_income = 1000,
                               has_partner = TRUE,
                               partner_is_pensioner = TRUE))
  expect_equal(youth_allowance(100,
                               age = c(18, 17, 22, 24, 66, 64),
                               partner_fortnightly_income = 1000,
                               has_partner = TRUE,
                               partner_is_pensioner = TRUE)[3],
               youth_allowance(100,
                               age = 22,
                               partner_fortnightly_income = 1000,
                               has_partner = TRUE,
                               partner_is_pensioner = TRUE))
  
})

test_that("Parenting payment", {
  expect_equal(youth_allowance(isjspceoalfofcoahodeoc = TRUE,
                               fy.year = "2016-17",
                               per = "fortnight"),
               750.3)
  expect_equal(youth_allowance(isjspceoalfofcoahodeoc = c(TRUE, FALSE),
                               fy.year = "2016-17",
                               per = "fortnight"),
               c(750.3, 442.35))
})


test_that("Youth allowance for multiple years", {
  expect_equal(youth_allowance(fy.year = yr2fy(2016:2018),
                               age = 16L,
                               per = "fortnight"),
               youth_allowance(fy.year = yr2fy(2016:2018),
                               age = 19L,
                               per = "fortnight"))
})

test_that("Manually set energy supplement", {
  # La plus ca meme
  expect_equal(youth_allowance(per = "fortnight",
                               fy.year = "2012-13",
                               has_partner = TRUE,
                               n_dependants = 0L,
                               include_ES = TRUE),
               youth_allowance(per = "fortnight",
                               fy.year = "2012-13",
                               include_ES = TRUE,
                               max_rate = 405.10,
                               es = 0))
  expect_equal(youth_allowance(per = "fortnight",
                               fy.year = "2013-14",
                               has_partner = TRUE,
                               n_dependants = 0L,
                               include_ES = TRUE),
               youth_allowance(per = "fortnight",
                               fy.year = "2013-14",
                               include_ES = TRUE,
                               max_rate = 410.95,
                               es = 3.5))
  expect_equal(youth_allowance(per = "fortnight",
                               fy.year = "2013-14",
                               has_partner = TRUE,
                               n_dependants = 0L,
                               include_ES = TRUE),
               youth_allowance(per = "fortnight",
                               fy.year = "2013-14",
                               include_ES = TRUE,
                               max_rate = 410.95,
                               es = 2.5) + 1)
})

test_that("When not eligible", {
  expect_equal(youth_allowance(400, 
                               fy.year = "2017-18",
                               per = "fortnight",
                               age = 21:30),
               if_else((21:30) <= 22, 448.65, 0))
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



