context("Medicare levy")

test_that("medicare_levy does not respond to invalid", {
  expect_error(medicare_levy(income = 123, fy.year = c("2015-16", "2015-17")))
  expect_error(medicare_levy(income = 123, fy.year = "2014-15", family_status = "couple"))
  expect_error(medicare_levy(income = 123, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 0, family_status = "individual"))
})

test_that("medicare_levy returns known values", {
  # https://www.ato.gov.au/calculators-and-tools/medicare-levy/
  
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 166.50)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 0)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 750, n_dependants = 0, family_status = "family"), 0)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 750, n_dependants = 0, family_status = "family"), 0)
  # 37000
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 740)
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 326.20)
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 750, n_dependants = 0, family_status = "family"), 159.90)
  # 46000
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 0, family_status = "family"), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 1, family_status = "family"), 749.30)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 2, family_status = "family"), 0)
  # 52000
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0, family_status = "individual"), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 0, family_status = "family"), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 1, family_status = "family"), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 1, family_status = "family"), 252.80)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 2, family_status = "family"), 0)
  
  
})

