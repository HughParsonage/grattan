context("Income tax in C++")

test_that("Equivalence in 2013-14 sample file", {
  skip_if_not_installed("taxstats")
  library(data.table)
  library(taxstats)
  s1314 <- copy(sample_file_1314)
  tx <- .subset2(s1314, "Taxable_Income")
  
  tax_rolling <- 
    rolling_income_tax(tx, "2013-14", .dots.ATO = s1314)
  
  tax_cpp <-
    income_tax_cpp(tx, "2013-14", .dots.ATO = s1314)
  
  expect_equal(tax_rolling, tax_cpp)
  
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