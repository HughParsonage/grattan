
context("Individual income tax")

test_that("income_tax returns known results",{
  
  expect_equal(income_tax(50e3, fy.year = "2012-13"), 8297)
  expect_equal(income_tax(60e3, fy.year = "2012-13"), 11847)
  expect_equal(income_tax(70e3, fy.year = "2012-13"), 15347)
  expect_equal(income_tax(200e3, fy.year = "2012-13"), 66547)
  
  expect_equal(income_tax(30e3, fy.year = "2013-14"), 2247)
  expect_equal(income_tax(40e3, fy.year = "2013-14"), 4747)
  expect_equal(income_tax(40e3, "2013-14", family_status = "family", n_dependants = 1L), 4394.70)
  # different rounding treatment.
  expect_equal(round(income_tax(40e3, "2013-14", family_status = "family", n_dependants = 0L, age = 66)), 2882)
  
  expect_equal(income_tax(31993, fy.year = "2014-15"), 2815.53)
  expect_equal(income_tax(31993, fy.year = "2014-15", age = 70), 0)
  
})

test_that("income_tax is not NA for any years)", {
  expect_false(any(is.na(income_tax(50e3, fy.year = yr2fy(2004:2015)))))
})

test_that("income_tax always returns the length of its arguments", {
  LEN <- ceiling(abs(rcauchy(1)))
  expect_equal(length(income_tax(runif(LEN, 0, 2e6), fy.year = sample(yr2fy(2004:2014), size = LEN, replace = TRUE))), LEN)
})

