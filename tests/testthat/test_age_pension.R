context("Age pension")

test_that("Maximum rates", {
  # http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
  expect_equal(age_pension(fy.year = "2015-16", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(fy.year = "2015-16", has_partner = TRUE), 15576.60)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = TRUE), 15576.60)
})

test_that("Means testing", {
  expect_equal(age_pension(fy.year = "2015-16",
                           has_partner = FALSE,
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           annual_income = c(0, 4000, 4264, 5264, 10374)), 
               c(20745.40, 
                 20745.40, 
                 20745.40,
                 20746.40 - 1000 * 0.5,
                 0))
  # http://guides.dss.gov.au/guide-social-security-law/4/2/3
  # Determine the reduction for assets
  # 
  # Divide the assets excess by 250.
  # Round the result DOWN to a whole number.
  # Multiply the whole number by 19.5.
  # RESULT: the REDUCTION FOR ASSETS.
  expect_equal(age_pension(fy.year = "2015-16", 
                           has_partner = FALSE, 
                           home_owner = FALSE,
                           # Assets limit 209,000: 
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 209e3, 210e3)),
               c(20745.40, 20745.40, 20745.40, 20745.40 - floor(1000/250) * 19.5))
})
