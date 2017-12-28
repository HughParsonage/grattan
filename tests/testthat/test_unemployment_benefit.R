context("Unemployment benefit")

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/10
test_that("DSS examples", {
  out <- 
    unemployment_benefit(income = 300, fy.year = date2fy("2017-09-20")) + 
    rent_assistance(fy.year = date2fy("2017-09-20"))
  
  expect_equal(out, 579.33)
    
})

test_that("Error handling", {
  expect_error(unemployment_benefit(income = 300, fy.year = "2015"),
               regexp = "fy.year.*which is not a valid")
  
  expect_error(unemployment_benefit(income = 300, fy.year = c("2015", "2015-16")),
               regexp = "fy.year.*contained invalid FYs")
  
  expect_error(unemployment_benefit(income = 300, fy.year = "1996-67"),
               regexp = "fy.year.*which is not a valid.*between")
  
  expect_error(unemployment_benefit(income = 300, fy.year = c("1995-96", "2015-16")),
               regexp = "fy.year.*were not within the allowed range")
})
