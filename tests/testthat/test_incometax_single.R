requireNamespace("grattan", quietly = TRUE)
requireNamespace("taxstats", quietly = TRUE)
requireNamespace("data.table", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)
requireNamespace("magrittr", quietly = TRUE)
context("Individual income tax")

test_that("income_tax returns known results",{
  
  expect_equal(income_tax(50e3, fy.year = "2012-13"), 8297)
  expect_equal(income_tax(60e3, fy.year = "2012-13"), 11847)
  expect_equal(income_tax(70e3, fy.year = "2012-13"), 15347)
  expect_equal(income_tax(200e3, fy.year = "2012-13"), 66547)
  
})

test_that("income_tax is not NA for years)", {
  expect_false(any(is.na(income_tax(50e3, fy.year = yr2fy(2013:2015)))))
})

