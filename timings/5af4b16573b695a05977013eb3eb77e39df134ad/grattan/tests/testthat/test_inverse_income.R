context("Test inverse income tax.")

test_that("Inverse single income matches.", {
  income <- sample(1:1e6, size = 1)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  expect_true(abs(inverse_income(income_tax(income, fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2, 
              info = paste0("income: ", income, "\n", "fy.year: ", fy.year))
})

test_that("Inverse long income matches.", {
  income <- sample(1:1e6, size = 2)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  expect_true(all(abs(inverse_income(income_tax(income, fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2), 
              info = paste0("income: ", paste0(income, collapse = " "), "\n", "fy.year: ", fy.year))
})