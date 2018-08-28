context("Income tax in C++")

test_that("Equivalence in 2013-14 sample file", {
  skip_if_not_installed("taxstats")
  library(data.table)
  s1314 <- copy(sample_file_1314)
  tx <- .subset2(s1314, "Taxable_Income")
  
  tax_rolling <- 
    rolling_income_tax(tx, "2013-14", .dots.ATO = s1314)
  
  tax_cpp <-
    income_tax_cpp(tx, "2013-14", .dots.ATO = s1314)
  
  expect_equal(tax_rolling, tax_cpp)
  
})
