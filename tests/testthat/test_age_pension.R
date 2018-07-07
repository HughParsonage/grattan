context("Age pension")

test_that("Error handling", {
  expect_warning(age_pension(Date = "2015-12-01", fy.year = "2015-16"), 
                 regexp = "Ignoring `fy.year`", 
                 fixed = TRUE)
})


test_that("Maximum rates", {
  # http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
  expect_equal(age_pension(fy.year = "2015-16", has_partner = FALSE), 20664.80)
  expect_equal(age_pension(fy.year = "2015-16", has_partner = TRUE), 15576.60)
  expect_equal(age_pension(Date = "2016-06-30", has_partner = FALSE), 20664.80)
  expect_equal(age_pension(Date = "2016-06-30", has_partner = TRUE), 15576.60)
})

test_that("Income means testing single", {
  expect_equal(age_pension(fy.year = "2015-16",
                           has_partner = FALSE,
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           annual_income = c(0, 4000, 4212, 5212, 50e3)), 
               c(20664.80, 
                 20664.80, 
                 20664.80,
                 20664.80 - 1000 * 0.5,
                 0))
})
  
test_that("Income means testing couple", {
  expect_equal(age_pension(fy.year = "2015-16",
                           has_partner = TRUE,
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           annual_income = c(0, 3000, 3744, 4744, 40e3)), 
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
                           is_home_owner = FALSE,
                           # Assets limit 360500
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 354.5e3, 364.5e3)),
               c(20664.80, 20664.80, 20664.80 - floor(10000/250) * 19.5))
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
                           is_home_owner = TRUE,
                           # Assets limit 205,500: 
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 205500, 215500)),
               c(20664.80, 20664.80, 20664.80 - floor(10000/250) * 19.5))
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
                           is_home_owner = FALSE,
                           # Assets limit 440500:
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 440500, 446.5e3)),
               c(15576.60, 15576.60, 15576.60 - floor(6000/250) * 19.5))
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
                           is_home_owner = TRUE,
                           # Assets limit 291,500:
                           # http://guides.dss.gov.au/guide-social-security-law/4/10/3
                           assets = c(0, 291500, 292500)),
               c(15576.60, 15576.60, 15576.60 - floor(1000/250) * 19.5))
})
