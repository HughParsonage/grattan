context("Pension Supplement")

test_that("Warnings and Errors", {
  expect_warning(pension_supplement(),
                 reg_exp = '`Date` and `fy.year` not set, so using `Date` = "2016/03/01" and `fy.year` = "2015-16"')
  expect_warning(pension_supplement(fy.year = "2015-16"),
                 reg_exp = '`Date` not set. Using date as defined in fy2date()')
  expect_warning(pension_supplement(Date = as.Date("2016-07-01"), fy.year = "2016-17"),
                 reg_exp = "`fy.year` and `Date` both used. Ignoring `fy.year`.")
  expect_error(suppressWarnings(pension_supplement(per = 'month')),
               reg_exp = "per can only take values `fortnight` or `annual`")
})

test_that("Rates", {
  #Max rates
  expect_equal(suppressWarnings(pension_supplement(has_partner = FALSE)),
            65)
  expect_equal(suppressWarnings(pension_supplement(has_partner = TRUE)),
            49)
  expect_equal(suppressWarnings(pension_supplement(age_pension_age_requirement = TRUE, age = 65)),
            65)
  #basic rates
  expect_equal(suppressWarnings(pension_supplement(has_partner = FALSE, parenting_payment = TRUE)),
            22.70)
  expect_equal(suppressWarnings(pension_supplement(has_partner = FALSE, overseas_absence = TRUE)),
            22.70)
  expect_equal(suppressWarnings(pension_supplement(has_partner = TRUE, overseas_absence = TRUE)),
            18.7)
})

test_that("Ineligibles", {
  expect_equal(suppressWarnings(pension_supplement(age_pension_age_requirement = TRUE, age = 20)),
            0)
  expect_equal(suppressWarnings(pension_supplement(disability_support_pension = TRUE, age = 20, n_dependents = 0)),
            0)
  expect_equal(suppressWarnings(pension_supplement(age_pension_age_requirement = TRUE, age = 65, Date = as.Date("2017/07/01"))),
            0)
})

test_that("Multiple People", {
  expect_equal(suppressWarnings(pension_supplement(has_partner = c(T,F))),
               c(49, 65))
})

