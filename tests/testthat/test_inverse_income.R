context("Test inverse income tax.")

test_that("Inverse single income matches.", {
  skip_on_cran()
  income <- sample(1:200e3, size = 1)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  
  # Ignore cases where the tax is zero
  while (income_tax(income, fy.year) < 1){
    income <- sample(1:200e3, size = 1)
    fy.year <- sample(yr2fy(2004:2015), size = 1)
  }
  
  expect_true(abs(inverse_income((income_tax(income, fy.year)), fy.year = fy.year, zero.tax.income = "maximum") - income) < 5/2, 
              info = paste0("income: ", income, "\n", "fy.year: ", fy.year))
  
  
  expect_equal(inverse_average_rate(0.097, "2003-04"), 15974, tol = 2)
  expect_error(inverse_average_rate(0.5, "2008-09", .max = 100), 
               regexp = "Stopping search")
})

test_that("Infinities", {
  expect_equal(inverse_income(Inf), Inf)
  expect_equal(inverse_income(c(NA, 10e3, Inf)), c(NA, 54798, Inf))
})

test_that("Inverse income on zero", {
  expect_gt(income_tax(inverse_income(0, "2012-13", zero.tax.income = "maximum") + 1, "2012-13"), 0)
  expect_equal(inverse_income(0, "2014-15", zero.tax.income = 5), 5)
  expect_equal(inverse_income(0, "2015-16", zero.tax.income = "zero"), 0)
  expect_error(inverse_income(-1, "2015-16"))
  expect_lte(inverse_income(0, "2014-15", zero.tax.income = "uniform"), inverse_income(0, "2014-15", zero.tax.income = "maximum"))
})

test_that("Inverse long income matches.", {
  skip_on_cran()
  income <- sample(1:300e3, size = 2)
  fy.year <- sample(yr2fy(2004:2015), size = 1)
  while (any(income_tax(income, fy.year = fy.year) < 1)){
    income <- sample(1:300e3, size = 2)
    fy.year <- sample(yr2fy(2004:2015), size = 1)
  }
  
  expect_true(all(abs(inverse_income(income_tax(income, fy.year),
                                     fy.year = fy.year,
                                     zero.tax.income = "zero") - income) <= 2), 
              info = paste0("income <- c(", paste0(income, collapse = ", "), ")\n", "fy.year <- '", fy.year, "'"))
})

test_that("Previously failed long income matches.", {
  skip_on_cran()
  income <- c(377840, 925203) #sample(1:1e6, size = 2)
  fy.year <- "2014-15" #  sample(yr2fy(2004:2015), size = 1)
  expect_true(all(abs(inverse_income(income_tax(income, fy.year = fy.year), fy.year = fy.year, zero.tax.income = "maximum") - income) < 1/2), 
              info = paste0("income <- c(", paste0(income, collapse = ", "), ")", "\n", "fy.year <- ", fy.year))
})

test_that("Inverse income long on zero", {
  skip_on_cran()
  expect_true(all(abs(inverse_income(c(0, 0), "2012-13", zero.tax.income = "maximum") - 20542) < 2))
  expect_true(all(inverse_income(c(0, 0), "2014-15", zero.tax.income = 5) %>% near(5)))
  expect_true(all(inverse_income(c(0, 0), "2014-15", zero.tax.income = "zero") %>% dplyr::near(0)))
  expect_true(all(inverse_income(c(0, 0), "2014-15", zero.tax.income = "uniform") %>% between(0, 20543)))
})

test_that("Other arguments passed to income_tax", {
  expect_equal(25e3, inverse_income_lookup3(income_tax(25e3, "2013-14", age = 70),
                                            fy.year = "2013-14",
                                            age = 70), 
               tol = 5/2)
  
})

