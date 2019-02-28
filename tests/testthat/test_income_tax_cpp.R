context("Income tax in C++")

test_that("Error handling", {
  expect_error(income_tax_cpp(80e3, c("2015-16", "2014-15")), 
               regexp = "`fy.year` was length-2.", 
               fixed = TRUE)
  expect_error(income_tax_cpp(80e3, "2010-11"), 
               regexp = "2012-13 onwards", 
               fixed = TRUE)
  
  expect_error(income_tax_cpp(80e3, "2030-31"), 
               regexp = "2012-13 onwards", 
               fixed = TRUE)
})

test_that("Equivalence in 2013-14 sample file", {
  skip_if_not_installed("taxstats")
  library(data.table)
  library(taxstats)
  s1314 <- copy(sample_file_1314)
  tx <- .subset2(s1314, "Taxable_Income")
  
  rep_along <- function(x, y) {
    rep_len(x, length(y))
  }
  
  for (fy in c("2013-14", "2014-15", "2015-16", "2016-17", 
               "2017-18", "2018-19", "2019-20")) {
    tax_rolling <- 
      rolling_income_tax(tx, fy, .dots.ATO = s1314)
    
    tax_cpp <-
      income_tax_cpp(tx, fy, .dots.ATO = s1314)
    
    expect_equal(tax_rolling, tax_cpp, info = fy)
  }
  
  s1314[, "Birth_year" := s1314$age_range]
  
  tax_cpp2 <-
    income_tax_cpp(tx, "2013-14", .dots.ATO = s1314)
  tax_rolling <- 
    rolling_income_tax(tx, "2013-14", .dots.ATO = s1314)
  expect_equal(tax_rolling, tax_cpp2)
  
  s1314[, "age_range" := NULL]
  
  tax_cpp3 <-
    income_tax_cpp(tx, "2013-14", .dots.ATO = s1314)
  expect_equal(tax_rolling, tax_cpp3)
  s1314[, age_range := 0L]
  expect_equal(income_tax(tx, rep_along("2013-14", tx), .dots.ATO = s1314), 
               income_tax_cpp(tx, "2013-14", .dots.ATO = s1314))
  
  expect_equal(income_tax(tx, rep_along("2013-14", tx), .dots.ATO = s1314), 
               income_tax_cpp(tx, "2013-14", .dots.ATO = s1314))
  
  # For coverage
  s1314[, Sw_amt := as.double(Sw_amt)]
  expect_equal(income_tax(tx, rep_along("2013-14", tx), .dots.ATO = s1314), 
               income_tax_cpp(tx, "2013-14", .dots.ATO = s1314))
  
  expect_equal(income_tax(tx, rep_along("2013-14", tx), age = 50 + 50 * s1314$Gender, .dots.ATO = NULL), 
               income_tax_cpp(tx, "2013-14", sapto.eligible = as.logical(s1314$Gender), .dots.ATO = NULL))
  
  
})

test_that("Single sapto.eligible", {
  expect_equal(income_tax_cpp(50e3, "2013-14", sapto.eligible = TRUE), 
               rolling_income_tax(50e3, "2013-14", age = 70))
})


test_that("sapto_rcpp", {
  out <- 
    sapto_rcpp(c(25e3, 35e3),
               MaxOffset = c(2e3, 3e3),
               LowerThreshold = c(20e3, 20e3),
               TaperRate = c(0.125, 0.125),
               SaptoEligible = c(TRUE, FALSE),
               SpouseIncome = c(0, 0),
               IsMarried = c(FALSE, TRUE))
  
  expect_equal(out, c(1375, 0))
})
