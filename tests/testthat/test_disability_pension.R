context("Disability")

test_that("Temporary", {
  expect_error(disability_pension())
  expect_error(disability_pension(fy.year = "2016-17"))
})

test_that("Disability results", {
  expect_equal(disability_pension(fy.year = "2015-16"), 
               age_pension(fy.year = "2015-16"))
})

