context("standardize payment names") 

test_that("pension", {
  expect_equal(standardize_payment_names("age pension"), 
               "age pension")
})