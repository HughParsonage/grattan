context("Pension Supplement")

test_that("Warnings and Errors", {
  expect_message(pension_supplement(),
                 reg_exp = '`Date` and `fy.year` not set, so using `Date` = "2016/03/01" and `fy.year` = "2015-16"')
  expect_error(suppressWarnings(pension_supplement(per = 'month')),
               reg_exp = "per can only take values `fortnight` or `annual`")
  expect_error(suppressWarnings(pension_supplement(has_partner = FALSE, seperated_couple = TRUE)),
               reg_exp = "incompatible values of `has_partner` and `partner_seperated`")
})

test_that("Rates", {
  #Max rates
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = FALSE),
               65)
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = TRUE),
               49)
  expect_equal(pension_supplement(Date = "2016/03/01", qualifying_payment = 'age_pension', age = 65),
               65)
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = TRUE, seperated_couple = TRUE),
               65)
  #basic rates
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = FALSE, parenting_payment = TRUE),
               22.70)
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = FALSE, overseas_absence = TRUE),
               22.70)
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = TRUE, overseas_absence = TRUE),
               18.7)
  
})

test_that("Ineligibles", {
  expect_equal(pension_supplement(Date = "2016/03/01", qualifying_payment = 'austudy', age = 20),
               0)
  expect_equal(pension_supplement(Date = "2016/03/01", qualifying_payment = 'disability_support_pension', age = 20, n_dependants = 0),
               0)
  expect_equal(pension_supplement(Date = "2018/03/01", qualifying_payment = 'partner_allowance', age = 65),
               0)
  expect_equal(pension_supplement(Date = "2018/03/01", qualifying_payment = 'youth_allowance', age = 20),
               0)
})

test_that("Multiple People", {
  expect_equal(pension_supplement(Date = "2016/03/01", has_partner = c(TRUE, FALSE)),
               c(49, 65))
})

