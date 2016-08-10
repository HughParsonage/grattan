context("Test inverse income tax.")

test_that("Inverse single income matches.", {
  income <- sample(1:1e6, size = 1)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  expect_true(abs(inverse_income(income_tax(income, fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2, 
              info = paste0("income: ", income, "\n", "fy.year: ", fy.year))
  
  expect_true(abs(inverse_income_while(income_tax(income, fy.year), fy.year = fy.year) - income) < 3/2, 
              info = paste0("income: ", income, "\n", "fy.year: ", fy.year))
})

test_that("Inverse income on zero", {
  expect_gt(income_tax(inverse_income(0, "2012-13", zero.tax.income = "maximum") + 1, "2012-13"), 0)
  expect_equal(inverse_income(0, "2014-15", zero.tax.income = 5), 5)
  expect_equal(inverse_income(0, "2015-16", zero.tax.income = "zero"), 0)
  expect_error(inverse_income(-1, "2015-16"))
})

test_that("Inverse long income matches.", {
  income <- sample(1:1e6, size = 2)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  expect_true(all(abs(inverse_income(income_tax(income, fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2), 
              info = paste0("income <- ", paste0(income, collapse = " "), "\n", "fy.year <- ", fy.year))
})

test_that("Previously failed long income matches.", {
  income <- c(377840, 925203) #sample(1:1e6, size = 2)
  fy.year <- "2014-15" #  sample(yr2fy(2004:2015), size = 1)
  expect_true(all(abs(inverse_income(income_tax(income, fy.year = fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2), 
              info = paste0("income <- c(", paste0(income, collapse = ", "), ")", "\n", "fy.year <- ", fy.year))
})