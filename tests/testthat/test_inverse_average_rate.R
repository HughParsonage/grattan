context("Inverse average tax rate")

test_that("Error handling", {
  expect_error(inverse_average_rate(1:2, fy.year = "2013-14", age = 40))
})


test_that("Average tax rates correctly inverted", {
  skip_on_cran()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- sample(yr2fy(2004:2016), size = 1)
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2003-04", {
  skip_on_cran()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2003-04"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2004-05", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2004-05"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2005-06", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2005-06"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2006-07", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2006-07"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2007-08", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2007-08"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2008-09", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2008-09"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2009-10", {
  skip_on_cran(); skip_on_travis(); skip_on_appveyor()
  skip_on_circleci(1)
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  fy.year <- "2009-10"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2010-11", {
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  skip_on_circleci(1)
  fy.year <- "2010-11"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2011-12", {
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  skip_on_circleci(1)
  fy.year <- "2011-12"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2012-13", {
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  skip_on_circleci(1)
  fy.year <- "2012-13"
  age = sample(c(42, 67), size = 1)
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

test_that("Average tax rates correctly inverted for 2013-14", {
  income <- ceiling(abs(rlnorm(1, 11, 1)) + 1)
  skip_on_circleci(1)
  fy.year <- "2013-14"
  age = sample(c(42, 67), size = 1)
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

