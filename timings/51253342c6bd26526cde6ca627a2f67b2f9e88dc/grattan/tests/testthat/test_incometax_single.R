
context("Individual income tax")

test_that("income_tax returns known results",{
  
  expect_equal(income_tax(50e3, fy.year = "2012-13"), 8297)
  expect_equal(income_tax(60e3, fy.year = "2012-13"), 11847)
  expect_equal(income_tax(70e3, fy.year = "2012-13"), 15347)
  expect_equal(income_tax(200e3, fy.year = "2012-13"), 66547)
  
})

test_that("income_tax is not NA for any years)", {
  expect_false(any(is.na(income_tax(50e3, fy.year = yr2fy(2004:2015)))))
})

test_that("income_tax always returns the length of its arguments", {
  LEN <- ceiling(abs(rcauchy(1)))
  expect_equal(length(income_tax(runif(LEN, 0, 2e6), fy.year = sample(yr2fy(2004:2014), size = LEN, replace = TRUE))), LEN)
})

