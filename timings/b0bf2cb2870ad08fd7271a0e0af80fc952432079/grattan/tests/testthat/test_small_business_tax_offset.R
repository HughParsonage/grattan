context("Small business tax offset")

test_that("Example in explanatory memo", {
  expect_equal(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = "2015-16"),
               625)
})

test_that("Always zero for fy_year", {
  expect_equal(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = "2014-15"),
               0)
})

test_that("Error handling", {
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3),
               regexp = "`fy_year` and `tax_discount` are both NULL")
  
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3, 
                                         tax_discount = "0.05"),
               regexp = "tax_discount. was class 'character'")
  
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         tax_discount = c(0.05, 0.08)),
               regexp = "Provide a single value or a value for every observation.")
  
  expect_warning(small_business_tax_offset(100e3,
                                           basic_income_tax_liability = 25e3,
                                           aggregated_turnover = 1e6,
                                           total_net_small_business_income =  50e3,
                                           fy_year = "2015-16",
                                           tax_discount = 0.05),
                 regexp = "fy_year.*tax_discount")
  
  
})
