context("Age pension")

test_that("Maximum rates", {
  # http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
  expect_equal(age_pension(fy.year = "2015-16", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(fy.year = "2015-16", has_partner = TRUE), 15576.60)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = TRUE), 15576.60)
})

test_that("Income means testing single", {
  expect_equal(age_pension(fy.year = "2015-16",
                           has_partner = FALSE,
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           annual_income = c(0, 4000, 4264, 5264, 10374)), 
               c(20745.40, 
                 20745.40, 
                 20745.40,
                 20746.40 - 1000 * 0.5,
                 0))
})
  
test_that("Income means testing couple", {
  expect_equal(age_pension(fy.year = "2015-16",
                           has_partner = TRUE,
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           annual_income = c(0, 3000, 3796, 4796, 7789)), 
               c(15576.60, 
                 15576.60, 
                 15576.60,
                 15576.60 - 1000 * 0.5,
                 0))
})
  
test_that("Assets means testing single homeowner", {
  # http://guides.dss.gov.au/guide-social-security-law/4/2/3
  # Determine the reduction for assets
  # 
  # Divide the assets excess by 250.
  # Round the result DOWN to a whole number.
  # Multiply the whole number by 19.5.
  # RESULT: the REDUCTION FOR ASSETS.
  expect_equal(age_pension(fy.year = "2015-16", 
                           has_partner = FALSE, 
                           home_owner = TRUE,
                           # Assets limit 360500
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 360.5e3, 365.5e3)),
               c(20745.40, 20745.40, 20745.40 - floor(1000/250) * 19.5))
})

test_that("Assets means testing single non-homeowner", {
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
               c(20745.40, 20745.40, 20745.40 - floor(1000/250) * 19.5))
})

test_that("Assets means testing couple non-homeowner", {
  # http://guides.dss.gov.au/guide-social-security-law/4/2/3
  # Determine the reduction for assets
  # 
  # Divide the assets excess by 250.
  # Round the result DOWN to a whole number.
  # Multiply the whole number by 19.5.
  # RESULT: the REDUCTION FOR ASSETS.
  expect_equal(age_pension(fy.year = "2015-16", 
                           has_partner = TRUE, 
                           home_owner = FALSE,
                           # Assets limit 448,000:
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 448e3, 450e3)),
               c(15576.60, 15576.60, 15576.60 - floor(2000/250) * 19.5))
})

test_that("Assets means testing couple homeowner", {
  # http://guides.dss.gov.au/guide-social-security-law/4/2/3
  # Determine the reduction for assets
  # 
  # Divide the assets excess by 250.
  # Round the result DOWN to a whole number.
  # Multiply the whole number by 19.5.
  # RESULT: the REDUCTION FOR ASSETS.
  expect_equal(age_pension(fy.year = "2015-16", 
                           has_partner = TRUE, 
                           home_owner = TRUE,
                           # Assets limit 296,500:
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 296500, 298500)),
               c(15576.60, 15576.60, 15576.60, 15576.60 - floor(2000/250) * 19.5))
})
