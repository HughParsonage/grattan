context("Unemployment benefit")

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/10
test_that("DSS examples", {
  out <- unemployment_benefit(income = 300, fy.year = date2fy("2017-09-20"))
    
})