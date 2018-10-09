context("newstart allowance")

test_that("Errors", {
  expect_error(newstart_allowance(fy.year = "2011-12"),
                 regexp = '`fy.year` can only take value "2015-16" for now')
  expect_error(newstart_allowance(per = "month"),
               regexp = "`per = 'month'` but must be one of 'year', 'fortnight', or 'quarter'.",
               fixed = TRUE)
  expect_error(newstart_allowance(fortnightly_income = 200, annual_income = 100000),
               regexp = 'input for `annual_income` is not 26 times larger than `fortnightly_income`')
  expect_error(newstart_allowance(has_partner = TRUE,
                                  fortnightly_partner_income = 200,
                                  annual_partner_income = 100000),
               regexp = 'input for `annual_partner_income` is not 26 times larger than `fortnightly_partner_income`')
  expect_error(newstart_allowance(fortnightly_income = c(1, 2, 3), age = c(22, 23)),
               regexp = '`age` had length 2, but the length of `fortnightly_income` is 3.',
               fixed = TRUE)
  expect_error(newstart_allowance(has_partner = FALSE, partner_pensioner = TRUE),
               regexp = 'check conflicting values for `has_partner` and `partner_pensioner`')
  expect_error(newstart_allowance(annual_partner_income = 100),
               regexp = 'check conflicting values for `has_partner` and `annual_partner_income`')
  expect_error(newstart_allowance(fortnightly_partner_income = 100),
               regexp = 'check conflicting values for `has_partner` and `fortnightly_partner_income`')
})

test_that("Correct values, no income", {
  #values retrieved for singles: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #values retrieved for couples: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
  #additional source from wayback machine which contains isjspceoalfofcoahodeoc value: https://web.archive.org/web/20160502192819/http://guides.dss.gov.au/guide-social-security-law/5/1/8/20
  #using 20/03/2016 values, note that rate is indexed in March and September each year.
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 22,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               527.6)
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 22,
                                  per = "fortnight",
                                  n_dependants = 1L,
                                  fy.year = "2015-16"),
               570.8)
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 60,
                                  per = "fortnight",
                                  nine_months = TRUE,
                                  fy.year = "2015-16"),
               570.8)
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 22,
                                  per = "fortnight",
                                  has_partner = TRUE,
                                  fy.year = "2015-16"),
               476.4)
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 22,
                                  per = "fortnight",
                                  has_partner = TRUE,
                                  n_dependants = 1L,
                                  fy.year = "2015-16"),
               476.4)
  expect_equal(newstart_allowance(fortnightly_income = 0,
                                  age = 22,
                                  per = "fortnight",
                                  isjspceoalfofcoahodeoc = TRUE,
                                  fy.year = "2015-16"),
               737.10)
})

#examples for singles taken from http://guides.dss.gov.au/guide-social-security-law/5/5/2
  #subtracted extra benefits (RA, ES, ...)
test_that("Correct values, income", {
  expect_equal(newstart_allowance(fortnightly_income = 300,
                                  age = 22,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               423.8) 
  expect_equal(newstart_allowance(fortnightly_income = 300,
                                  age = 22,
                                  has_partner = FALSE,
                                  n_dependants = 1L,
                                  per = "fortnight",
                                  principal_carer = TRUE,
                                  fy.year = "2015-16"),
               491.6)
  #examples for partners http://guides.dss.gov.au/guide-social-security-law/5/5/3
    #2015-16 values taken from wayback machine
  expect_equal(newstart_allowance(fortnightly_income = 290,
                                  age = 22,
                                  has_partner = TRUE,
                                  fortnightly_partner_income = 160,
                                  n_dependants = 1L,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               378.6)#example: /10 except es removed
  expect_equal(newstart_allowance(fortnightly_income = 140,
                                  age = 22,
                                  has_partner = TRUE,
                                  fortnightly_partner_income = 950,
                                  n_dependants = 1L,
                                  fy.year = "2015-16",
                                  per = "fortnight"),
               440)#example: /30 
})

test_that("Multiple people", {
  expect_equal(newstart_allowance(fortnightly_income = c(0,0), 
                                  n_dependants = c(0,1),
                                  age = 22,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               c(527.6, 570.8))
})

test_that("Elligibility and asset threshhold", {
  expect_equal(newstart_allowance(age = 20,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               0)
  expect_equal(newstart_allowance(age = 65,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               0)
  expect_equal(newstart_allowance(assets_value = 348000,
                                  per = "fortnight",
                                  fy.year = "2015-16"), 
               527.6)
  expect_equal(newstart_allowance(assets_value = 348501,
                                  per = "fortnight",
                                  fy.year = "2015-16"),
               0)
})

test_that("Per", {
  expect_equal(newstart_allowance(per = "annual"),
               13717.6)
})

test_that("Income annual", {
  expect_equal(newstart_allowance(annual_income = 10000, per = "fortnight"),
               373.0308, tolerance = 1e-3)
})

