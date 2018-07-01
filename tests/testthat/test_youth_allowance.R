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
                               age = 22,
                               eligible_if_over22 = TRUE,
                               living_at_home = TRUE),
               360.20)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 22,
                               eligible_if_over22 = TRUE,
                               living_at_home = FALSE),
               541.70)
  expect_equal(youth_allowance(ordinary_income = 0,
                               age = 22,
                               eligible_if_over22 = TRUE,
                               is_single = TRUE),
               541.70)
})

test_that("Income reduction"){
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
  expect_equal(income_reduction(ordinary_income = 864.84,
                                max_rate_March_2018 = 244.10,
                                max_income = 864.84,
                                is_student = TRUE),
               0.5 * (524 - 437) + 0.6 * (864.84 - 524))
  expect_equal(income_reduction())
}
