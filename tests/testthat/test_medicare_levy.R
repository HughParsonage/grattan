context("Medicare levy")

test_that("Medicare requires one spouse income or one per record", {
  incomes <- rep(30000L, 3)
  expect_equal(medicare_levy(incomes, "2023-24", Spouse_income = 23000L), rep(255.4, 3))
  expect_equal(medicare_levy(incomes, "2023-24", Spouse_income = rep(23000L, 3)), rep(255.4, 3))
  for (n in c(0L, 2L, 4L)) {
    expect_error(medicare_levy(incomes, "2023-24", Spouse_income = rep(23000L, n)), "Spouse_income")
  }
  expect_identical(medicare_levy(integer(), "2023-24", Spouse_income = integer()), numeric())
  expect_identical(medicare_levy(integer(), "2023-24", Spouse_income = 0L), numeric())
  expect_error(medicare_levy(integer(), "2023-24", Spouse_income = c(0L, 0L)), "Spouse_income")
})

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

test_that("2020-21 SAPTO Medicare thresholds retain the full-rate boundary", {
  # 2021 Measures No.3 EM Table1.1: reduced levy through $45,881,
  # full levy from $45,882. The upper-threshold API returns the latter.
  expect_equal(ML_LWR_THRESH(2021, family = FALSE, sapto = TRUE), 36705L)
  expect_equal(ML_UPR_THRESH(2021, family = FALSE, sapto = TRUE), 45882L)
})

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
  expect_equal(medicare_levy(20e3, "2004-05", sato = NULL, pto = FALSE), 300)
  expect_equal(medicare_levy(20e3, "2004-05", sato = TRUE), 0)
  expect_equal(medicare_levy(16e3, "2001-02"), 240)
  expect_equal(medicare_levy(16e3, "2001-02", sapto.eligible = TRUE), 0)
})

test_that("Medicare family income 2015-16", {
  # Medicare Levy Act ss7-8, with the 2016 Act No.39 annual thresholds.
  expect_equal(medicare_levy(income = 37e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 750, n_dependants = 0), 159.90)
  expect_equal(medicare_levy(income = 46e3, fy.year = "2015-16", sapto.eligible = FALSE, Spouse_income = 1e3, n_dependants = 1), 749.30)
  expect_equal(medicare_levy(income = 52e3, fy.year = "2015-16", sapto.eligible = TRUE , Spouse_income = 1e3, n_dependants = 1), 252.80)
  
  expect_equal(medicare_levy(17630, Spouse_income = 17630, fy.year = "2015-16"), 0)
  expect_equal(medicare_levy(18000, Spouse_income = 18000, fy.year = "2015-16"), 0)
})

test_that("Medicare with dependants", {
  # Issue #50's calculator examples, now active; compare amounts in cents.
  levy <- function(income, fy.year, Spouse_income, n_dependants) {
    round(medicare_levy(income, fy.year, Spouse_income = Spouse_income,
                        n_dependants = n_dependants, is_married = TRUE), 2)
  }
  expect_equal(levy(26000, '2014-15', 26000, 1), 510.40)
  expect_equal(levy(26000, '2014-15', 26000, 2), 503.55)
  expect_equal(levy(26000, '2015-16', 26000, 1), 466.50)
  expect_equal(levy(24000, '2015-16', 28000, 3), 67.16)
  expect_equal(levy(26000, '2015-16', 26000, 2), 415.85)
  expect_equal(levy(26000, '2015-16', 26000, 3), 250.55)
  expect_equal(levy(26001, '2015-16', 26000, 3), 250.69)
  expect_equal(levy(26001, '2015-16', 26001, 3), 250.73)
  expect_equal(levy(26100, '2015-16', 26100, 3), 268.55)
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







