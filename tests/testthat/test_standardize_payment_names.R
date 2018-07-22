context("standardize payment names") 

test_that("pension", {
  expect_equal(standardize_payment_names("age pension"), 
               "age pension")
  expect_equal(standardize_payment_names(c("age pension", 
                                           "age pension", 
                                           "nsa", 
                                           "age pension",
                                           "nsa")), 
               c("age pension", 
                 "age pension", 
                 "newstart allowance",
                 "age pension",
                 "newstart allowance"))
})