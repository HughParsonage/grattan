context("Youth allowance")

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/40
test_that("Allowance with no income", {
  expect_equal(youth_allowance(ordinary_income = 0,
                               age=16,
                               is_single = TRUE,
                               living_at_home = TRUE),
               244.10)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age=18,
                               is_single = TRUE,
                               living_at_home = TRUE),
               293.60)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age=17,
                               is_single = TRUE,
                               has_children = FALSE,
                               living_at_home = FALSE),
               445.80)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 16,
                               is_single = TRUE,
                               has_children = TRUE),
               584.20)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 20,
                               isjspceoalfofcoahodeoc = TRUE),
               762.40)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 20,
                               is_single = FALSE,
                               has_children = FALSE),
               445.80)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 28,
                               is_single = FALSE, 
                               has_children = TRUE),
               489.60)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 22,
                               eligible_if_over22 = TRUE,
                               living_at_home = TRUE),
               360.20)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 25,
                               eligible_if_over22 = TRUE,
                               living_at_home = FALSE),
               541.70)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 25,
                               eligible_if_over22 = TRUE,
                               is_single = FALSE),
               489.60)
})

test_that("Income reduction", {
  #max income data: https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
  #Student
  expect_equal(income_reduction(ordinary_income = 300,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0)
  expect_equal(income_reduction(ordinary_income = 500,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (500 - 437))
  expect_equal(income_reduction(ordinary_income = 700,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (700-524))
  expect_equal(income_reduction(ordinary_income = 860,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (860-524))
  
  #Jobseeker
  expect_equal(income_reduction(ordinary_income = 300,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0)
  expect_equal(income_reduction(ordinary_income = 500,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (500 - 437))
  expect_equal(income_reduction(ordinary_income = 700,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (700-524))
  expect_equal(income_reduction(ordinary_income = 860,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (860-524))##ERROR: REDUCES BY MORE THAN RATE)
})

#http://guides.dss.gov.au/guide-social-security-law/5/5/2
#test_that('Allowance with income reduction',{
#  expect_equal()
#})

test_that('Income reduction not exceed max rate', {
  skip('threshhold on https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411 contradicts taper rates')
  expect_lt(income_reduction(ordinary_income = 860,
                             max_rate_March_2018 = 244.10,
                             max_income = 864.84,
                             is_student = TRUE),
            244.10)
  expect_equal(income_reduction(ordinary_income = 864.84,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (864.84 - 524))
  expect_equal(income_reduction(ordinary_income = 864.84,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (864.84 - 524))
})