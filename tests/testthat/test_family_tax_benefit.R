context("Family tax Benefit")

test_that("Benefit with no income", {
  expect_equal(family_tax_benefit(child_age = 0),
               179.76)
  expect_equal(family_tax_benefit(child_age = 15),
               233.94)
  expect_equal(family_tax_benefit(child_age = 18, child_in_secondary_school = TRUE),
               233.94)
  expect_equal(family_tax_benefit(child_age = 19, child_in_care = TRUE),
               57.68)
})