context("Inverse average tax rate")

test_that("Average tax rates correctly inverted", {
  expect_error(inverse_average_rate(1:2, fy.year = "2013-14", age = 40))
  
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- sample(yr2fy(2004:2016), size = 1)
  age = sample(20:80, size = 1)
  actual_average_tax_rate <- income_tax(income, fy.year = fy.year, age = age) / income
  if (!(actual_average_tax_rate > 0)){
    expect_error(inverse_average_rate(actual_average_tax_rate, fy.year = fy.year, age = age))
  } else {
    guessed_income <- inverse_average_rate(actual_average_tax_rate, fy.year = fy.year, age = age)
    expect_true(abs(guessed_income - income) <= 1, 
                info = paste0("income = ",  income, "\n",
                              "fy.year = '", fy.year, "'\n", 
                              "age = ", age, "\n"))
  }
})