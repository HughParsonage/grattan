context("Child Care Subsidy")

test_that("Errors", {
  expect_error(child_care_subsidy(type_of_day_care = "kjgkj"),
               reg_exp = "'arg' should be one of ",
               fixed = TRUE)
  expect_error(child_care_subsidy(family_annual_income = "100000"),
               reg_exp = "`family_annual_income` was type character but must be numeric.")
  expect_error(child_care_subsidy(activity_level = "100000"),
               reg_exp = "`activity_level` was type character but must be numeric.")
  expect_error(child_care_subsidy(activity_exemption = "TRUE"),
               reg_exp = "`activity_exemption` was type character but must be logical")
  expect_error(child_care_subsidy(child_age = "2"),
               reg_exp = "`child_age` was type character but must be numeric.")
  expect_error(child_care_subsidy(hours_day_care_fortnight = "2"),
               reg_exp = "`hours_day_care_fortnight` was type character but must be numeric.")
  expect_error(child_care_subsidy(cost_hour = "2"),
               reg_exp = "`cost_hour` was type character, but must be numeric.")
  expect_error(child_care_subsidy(early_education_program = "TRUE"),
               reg_exp = "`early_education_program` was type character, but must be logical")
})

test_that("Values", {
  expect_equal(child_care_subsidy(family_annual_income = 175000,
                     activity_level = 40,
                     activity_exemption = FALSE,
                     child_age = 3,
                     type_of_day_care = "cbdc",
                     cost_hour = 20,
                     hours_day_care_fortnight = 80,
                     early_education_program = FALSE),
               11046.99,
               tol = 0.01)
  # Income tests
  expect_equal(child_care_subsidy(family_annual_income = 150000,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.58 * 365/14)
  expect_equal(child_care_subsidy(family_annual_income = 200000,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.5 * 365/14)
  expect_equal(child_care_subsidy(family_annual_income = 300000,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.34 * 365/14)
  expect_equal(child_care_subsidy(family_annual_income = 350000,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.2 * 365/14)
  expect_equal(child_care_subsidy(family_annual_income = 400000,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.0 * 365/14)
  # Childcare adjustments
  expect_equal(child_care_subsidy(type_of_day_care = "cbdc",
                                  child_age = 10,
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               child_care_subsidy(type_of_day_care = "oshc",
                                  child_age = 10,
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20))
  expect_equal(child_care_subsidy(type_of_day_care = "oshc",
                                  child_age = 5,
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               child_care_subsidy(type_of_day_care = "cbdc",
                                  child_age = 5,
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20))
  # Types of childcare
  expect_equal(child_care_subsidy(type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               11.77 * 0.85 * 365/14)
  expect_equal(child_care_subsidy(type_of_day_care = "fdc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               10.9 * 0.85 * 365/14)
  expect_equal(child_care_subsidy(type_of_day_care = "oshc",
                                  child_age = 7,
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 20),
               10.29 * 0.85 * 365/14)
  expect_equal(child_care_subsidy(type_of_day_care = "ihc",
                                  hours_day_care_fortnight = 1,
                                  cost_hour = 30),
               25.48 * 0.85 * 365/14)
  # Annual cap
  expect_equal(child_care_subsidy(family_annual_income = 200000,
                               type_of_day_care = "cbdc",
                               hours_day_care_fortnight = Inf,
                               cost_hour = 20),
               10190)
  # Activity test1
  expect_equal(child_care_subsidy(family_annual_income = 200000,
                     activity_level = 0,
                     type_of_day_care = "cbdc",
                     hours_day_care_fortnight = Inf,
                     cost_hour = 20),
               0)
  expect_equal(child_care_subsidy(family_annual_income = 200000,
                                  activity_level = 10,
                                  type_of_day_care = "cbdc",
                                  hours_day_care_fortnight = Inf,
                                  cost_hour = 20),
               36 * 11.77 * 0.5 * 365/14)

})