context("Child Care Subsidy")

test_that("Errors", {
  expect_error(child_care_subsidy(type_of_day_care = "kjgkj"),
               reg_exp = "'arg' should be one of “cbdc”, “oshc”, “fdc”, “ihc”")
  expect_error(child_care_subsidy(family_annual_income = "100000"),
               reg_exp = "`family_annual_income` was type character, but must be numeric.")
  expect_error(child_care_subsidy(activity_level = "100000"),
               reg_exp = "`activity_level` was type character, but must be numeric.")
  expect_error(child_care_subsidy(activity_exemption = "TRUE"),
               reg_exp = "`activity_exemption` was type character, but must be logical")
  expect_error(child_care_subsidy(child_age = "2"),
               reg_exp = "`child_age` was type character, but must be numeric.")
  expect_error(child_care_subsidy(hours_day_care_fortnight = "2"),
               reg_exp = "`hours_day_care_fortnight` was type character, but must be numeric.")
  expect_error(child_care_subsidy(cost_hour = "2"),
               reg_exp = "`cost_hour` was type character, but must be numeric.")
  expect_error(child_care_subsidy(early_education_program = "TRUE"),
               reg_exp = "`early_education_program` was type character, but must be logical")
})