context("Unemployment benefit")

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/10
test_that("DSS examples", {
  out <- 
    unemployment_benefit(income = 300, fy.year = date2fy("2017-09-20")) / 26 + 
    rent_assistance(fy.year = date2fy("2017-09-20")) 
  
  expect_equal(out, 579.33)
  
  outc <- 
    unemployment_benefit(income = 300, Date = "2017-09-20") + 
    rent_assistance(Date = "2017-09-20")
  
  outd <- 
    unemployment_benefit(income = 300, Date = as.Date("2017-09-20")) + 
    rent_assistance(Date = as.Date("2017-09-20"))
  
  expect_equal(outc, outd)
  expect_equal(outc, 578)
    
})

test_that("Error handling", {
  expect_error(unemployment_benefit(income = 300, fy.year = "2015"),
               regexp = "fy.year.*was not a valid")
  
  expect_error(unemployment_benefit(income = 300,
                                    fy.year = c("2016-17", "2015", "2015-16")),
               regexp = "fy.year.*contained invalid entry 2015 at position 2")
  
  expect_error(unemployment_benefit(income = 300, fy.year = "1996-67"),
               regexp = "fy.year.*was not a valid.*between")
  
  expect_error(unemployment_benefit(income = 300, fy.year = c("1995-96", "2015-16")),
               regexp = "`fy.year = 1995-96` was not within the allowed range:")
  
  expect_error(unemployment_benefit(income = 0, Date = "22"), 
               regexp = "neither a Date object nor safely coercible as such")
  expect_error(unemployment_benefit(income = 0, Date = c("1999-12-31")),
               regexp = "Ensure `Date` only includes dates between 2000 and 2020.",
               fixed = TRUE)
  
  
  expect_message(unemployment_benefit(), 
                 regexp = "`fy.year` not set")
  expect_error(unemployment_benefit(income = c(200, 300, 400), fy.year = c("2015-16", "2016-17")),
               regexp = "`fy.year` had length 2.")
  expect_error(unemployment_benefit(income = c(200, 300, 400), Date = fy2date(c("2015-16", "2016-17"))),
               regexp = "`Date` had length 2.")
})
