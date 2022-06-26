context("Medicare levy")

test_that("medicare_levy error handling / deprecated", {
  expect_error(medicare_levy(income = 123, fy.year = c("2015-16", "2015-17")))
  expect_warning(medicare_levy(income = 123, fy.year = "2014-15", family_status = "couple"),
                 regexp = "family_status")
})

test_that("medicare_levy monotonic", {
  expect_lte(medicare_levy(income = 29000,
                           fy.year = "2015-16",
                           Spouse_income = 29000,
                           sapto.eligible = TRUE), 
             medicare_levy(income = 30000,
                           fy.year = "2015-16",
                           Spouse_income = 30000,
                           sapto.eligible = TRUE))
} )

test_that("medicare_levy returns known values", {
  # https://www.ato.gov.au/calculators-and-tools/medicare-levy/
  expect_equal(medicare_levy(income = 40e3, fy.year = "2013-14", sapto.eligible = TRUE , n_dependants = 1), 0)
  expect_equal(medicare_levy(income = 40e3, fy.year = "2013-14", sapto.eligible = FALSE, n_dependants = 1), 247.7)
  
  
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0), 166.50)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0), 0)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 750, n_dependants = 0), 0)
  expect_equal(medicare_levy(income = 23e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 750, n_dependants = 0), 0)  
  # 37000
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0), 740)
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0), 326.20)
  
  # Proximity of thresholds 
  expect_equal(medicare_levy(income = 33739,
                             fy.year = "2015-16",
                             Spouse_income = 30000,
                             sapto.eligible = TRUE),
               0.1)
  
  # 46000
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 0), 920)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 2), 0)
  # 52000
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 0e3, n_dependants = 0), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 0e3, n_dependants = 0), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 0), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 1), 1040)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 2), 0)
  
  expect_equal(medicare_levy(20e3, "2004-05", sato = TRUE, pto = FALSE), 0)
  expect_equal(medicare_levy(20e3, "2004-05", sato = NULL, pto = FALSE), 0)
  expect_equal(medicare_levy(20e3, "2004-05", sato = TRUE), 0)
  expect_equal(medicare_levy(16e3, "2001-02"), 240)
  expect_equal(medicare_levy(16e3, "2001-02", sapto.eligible = TRUE), 0)
})

test_that("Medicare family income 2015-16", {
  skip("Government calculator wrong at 2017-02-18")
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 750, n_dependants = 0), 159.90)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 1), 749.30)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 1), 252.80)
  
  expect_equal(medicare_levy(17630, Spouse_income = 17630, fy.year = "2015-16"), 0)
  expect_equal(medicare_levy(18000, Spouse_income = 18000, fy.year = "2015-16"), 0)
})

test_that("Medicare with dependants", {
  skip("Issue 50 unresolved")
  expect_equal(medicare_levy(26000, fy.year = '2014-15', Spouse_income = 26000, n_dependants = 1, family_status = 'family'), 510.40)
  expect_equal(medicare_levy(26000, fy.year = '2014-15', Spouse_income = 26000, n_dependants = 2, family_status = 'family'), 503.55)
  
  expect_equal(medicare_levy(26000, fy.year = '2015-16', Spouse_income = 26000, n_dependants = 1, family_status = 'family'), 466.50)
  expect_equal(medicare_levy(24000, fy.year = '2015-16', Spouse_income = 28000, n_dependants = 3, family_status = 'family'), 67.16)
  expect_equal(medicare_levy(26000, fy.year = '2015-16', Spouse_income = 26000, n_dependants = 2, family_status = 'family'), 415.85)
  expect_equal(medicare_levy(26000, fy.year = '2015-16', Spouse_income = 26000, n_dependants = 3, family_status = 'family'), 250.55)
  expect_equal(medicare_levy(26001, fy.year = '2015-16', Spouse_income = 26000, n_dependants = 3, family_status = 'family'), 250.69)
  expect_equal(medicare_levy(26001, fy.year = '2015-16', Spouse_income = 26001, n_dependants = 3, family_status = 'family'), 250.73)
  expect_equal(medicare_levy(26100, fy.year = '2015-16', Spouse_income = 26100, n_dependants = 3, family_status = 'family'), 268.55)
})

test_that("Agrees with Master tax guide", {
  expect_equal(medicare_levy(44000, fy.year = "2014-15", Spouse_income = 0, n_dependants = 2), 226.30)
  expect_lte(abs(medicare_levy(29000, fy.year = "2014-15", Spouse_income = 27000, n_dependants = 4) - 403.25), 0.05)
  expect_lte(abs(medicare_levy(27000, fy.year = "2014-15", Spouse_income = 29000, n_dependants = 4) - 375.44), 0.05)
})

test_that("Medicare error handling", {
  expect_error(medicare_levy(20e3, "2004-05", sato = TRUE, pto = TRUE))
  expect_warning(medicare_levy(20e3, "2004-05", sato = FALSE, pto = TRUE), regexp = "pto")
})








