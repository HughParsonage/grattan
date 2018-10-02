context("Age pension")

test_that("Error handling", {
  expect_warning(age_pension(Date = "2015-12-01", fy.year = "2015-16"), 
                 regexp = "Ignoring `fy.year`", 
                 fixed = TRUE)
  expect_error(age_pension(fortnightly_income = 2,
                           annual_income = 1),
               regexp = "`fortnightly_income` is provided, yet `annual_income` is not 26 times its values.",
               fixed = TRUE)
  expect_error(age_pension(fortnightly_income = NA,
                           fy.year = "2015-16"),
               regexp = "`fortnightly_income` contains NAs.")
})


test_that("Maximum rates", {
  # http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
  expect_equal(age_pension(fy.year = "2015-16", has_partner = FALSE), 20664.80)
  expect_equal(age_pension(fy.year = "2015-16", has_partner = TRUE), 15576.60)
  expect_equal(age_pension(Date = "2016-06-30", has_partner = FALSE), 20664.80)
  expect_equal(age_pension(Date = "2016-06-30", has_partner = TRUE), 15576.60)
})

test_that("per message", {
  expect_message(age_pension(fy.year = "2015-16"), regexp = "per")
  expect_equal(age_pension(fy.year = "2015-16", per = "fortnight"), 
               age_pension(fy.year = "2015-16", per = "year") / 26)
  
  # undocumented but ok
  expect_equal(age_pension(fy.year = "2015-16", per = "fortnight"), 
               age_pension(fy.year = "2015-16", per = "annual") / 26)
  # undocumented but ok
  expect_equal(age_pension(fy.year = "2015-16", per = c("fortnight", "annual")), 
               age_pension(fy.year = "2015-16", per = "annual") / 26)
  
  # undocumented but ok
  expect_error(age_pension(fy.year = "2015-16", per = "fortnightly"),
               '`per` must be one of "fortnight" or "annual".', 
               fixed = TRUE)
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
                           annual_income = c(0, 6000, 7488, 8488, 80e4)),
               c(15576.60, 
                 15576.60, 
                 15576.60,
                 15576.60 - 1000 * 0.25,
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
               c(15576.60, 15576.60, 15576.60 - 1/2 * floor(6000/250) * 19.5))
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
               c(15576.60, 15576.60, 15576.60 - 1/2 * floor(1000/250) * 19.5))
})

test_that("today helper", {
  expect_equal(.age_pension_today2qtr(as.Date("2017-07-01")), "2017-03-20")
  expect_equal(.age_pension_today2qtr("2017-07-01"), "2017-03-20")
  expect_equal(.age_pension_today2qtr("2017-03-21"), "2017-03-20")
  expect_equal(.age_pension_today2qtr(as.Date("2017-03-01")), "2016-09-20")
  expect_equal(.age_pension_today2qtr("2017-03-01"), "2016-09-20")
  expect_equal(.age_pension_today2qtr("2016-12-01"), "2016-09-20")
  expect_equal(.age_pension_today2qtr("2017-03-21"), "2017-03-20")
  expect_message(age_pension(), "not set")
})


test_that("guides.dss.gov.au deeming examples", {
  # http://guides.dss.gov.au/guide-social-security-law/4/4/1/60
  # John and Mary are both Age recipients with a combined total
  # of $90,000 in financial investments.  $25,000 is in a term 
  # deposit, $15,000 is in a credit union account and they have 
  # $50,000 worth of managed investments.
  expect_equal(age_pension(annual_income = 5e3,
                           Date = "2017-07-03",
                           has_partner = TRUE,
                           partner_pensioner = TRUE,
                           assets_value = 90e3,
                           financial_assets = 90e3), 
               age_pension(annual_income = 5e3 + 1674,
                           Date = "2017-07-03",
                           has_partner = TRUE,
                           partner_pensioner = TRUE,
                           assets_value = 90e3,
                           financial_assets = 0))
})
