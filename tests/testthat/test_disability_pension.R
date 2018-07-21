context("Disability")

test_that("Temporary", {
  expect_error(disability_pension())
  expect_error(disability_pension(fy.year = "2016-17"))
})

test_that("Error handling", {
  expect_error(disability_pension(fy.year = "2015-16", per = "foo"), 
               "must be one of.*fortnight.*year")
  expect_error(disability_pension(fortnightly_income = 1:10, 
                                  annual_income = 1:10,
                                  fy.year = "2015-16"), 
               regexp = "not equal to 26")
})

test_that("Disability results", {
  expect_equal(disability_pension(fy.year = "2015-16"), 
               age_pension(fy.year = "2015-16"))
  expect_equal(disability_pension(fortnightly_income = 1:200, fy.year = "2015-16"), 
               disability_pension(annual_income = 1:200 * 26, fy.year = "2015-16"))
})

test_that("per", {
  expect_message(disability_pension(fy.year = "2015-16"), 
                 "per")
  expect_equal(disability_pension(fy.year = "2015-16", per = "year"), 
               26 * disability_pension(fy.year = "2015-16", per = "fortnight"))
  expect_equal(disability_pension(fy.year = "2015-16", per = "annual"), 
               26 * disability_pension(fy.year = "2015-16", per = "fortnight"))
})

