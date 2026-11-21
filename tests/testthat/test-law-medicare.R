test_that("family Medicare reductions start from each section 7 liability", {
  # Medicare Levy Act 1986, sections 7 and 8(2)-(4).
  expect_equal(medicare_levy(30000, "2023-24", Spouse_income = 23000), 255.4)
  expect_equal(medicare_levy(45000, "2023-24", Spouse_income = 5000), 515.4)
  # Both spouses owe levy before the family reduction, but one cannot use
  # their allocated reduction. The excess must transfer to their spouse.
  expect_equal(medicare_levy(27000, "2023-24", Spouse_income = 26001), 0)
  expect_equal(medicare_levy(26001, "2023-24", Spouse_income = 27000), 0)
  # Issue #50: own section 7 levy is $266.50; the family reduction is
  # $431.90, of which 24000/52000 belongs to this taxpayer.
  expect_equal(medicare_levy(24000, "2015-16", Spouse_income = 28000,
                           n_dependants = 3),
               266.5 - 431.9 * 24000 / 52000)
})

test_that("Medicare uses spouse taxable income separately from rebate income", {
  ato <- data.table::data.table(ic_taxable_income_loss = 35000L,
                                c_age_30_june = 42L,
                                sp_flag = TRUE,
                                spc_rebate_income = 30000L,
                                sp_taxable_income_loss = 0L)
  expect_equal(income_tax(35000, "2023-24", .dots.ATO = ato), 2492)
  # Taxable losses do not reduce family taxable income below zero.
  expect_equal(medicare_levy(50000, "2023-24", Spouse_income = -20000,
                           is_married = TRUE), 615.4)
})

test_that("senior Medicare thresholds require actual SAPTO entitlement", {
  expect_equal(income_tax(35000, "2023-24", age = 66), 3192)
  # No SAPTO remains at $51,000, so the full $1,020 Medicare levy applies.
  expect_equal(income_tax(51000, "2023-24", age = 70), 7827)
  ato <- data.table::data.table(ic_taxable_income_loss = 45000L,
                                ic_rebate_income = 100000L,
                                c_age_30_june = 70L,
                                on_sapto_cd = "A")
  expect_equal(income_tax(45000, "2023-24", .dots.ATO = ato), 5667)
  # An explicitly blank claim remains ineligible even above pension age.
  ato <- data.table::data.table(ic_taxable_income_loss = 35000L,
                                c_age_30_june = 70L,
                                on_sapto_cd = "")
  expect_equal(income_tax(35000, "2023-24", .dots.ATO = ato), 3192)
  # Historical schedules without senior thresholds ignore the flag.
  expect_equal(medicare_levy(60000, "1999-00", sapto.eligible = TRUE), 900)
  expect_equal(income_tax(60000, "1999-00", age = 70), 19702)
})

test_that("Medicare vectors select each record's financial year", {
  expect_equal(medicare_levy(c(30000, 40000), c("2014-15", "2013-14")),
               c(600, 600))
  expect_equal(medicare_levy(c(30000, 40000), c("2013-14", "2014-15")),
               c(450, 800))
  expect_equal(medicare_levy(c(30000, 40000), "2014-15"), c(600, 800))
})

test_that("early Medicare rates, phase-in rules and ceilings are retained", {
  # Medicare Levy Acts 1983, 1984 and 1986, sections 6-8.
  expect_equal(medicare_levy(50000, "1983-84"), 208)
  expect_equal(medicare_levy(50000, "1984-85"), 500)
  expect_equal(medicare_levy(50000, "1985-86"), 500)
  expect_equal(medicare_levy(50000, "1986-87"), 572.5)
  expect_equal(medicare_levy(7000, "1983-84"), 0.0832 * (7000 - 6698))
  expect_equal(medicare_levy(7200, "1984-85"), 18)
  expect_equal(medicare_levy(100000, "1983-84"), 291.2)
  expect_equal(medicare_levy(100000, "1984-85"), 733.32)
  expect_equal(medicare_levy(50000, "1983-84", Spouse_income = 50000), 145.6)
  expect_equal(medicare_levy(50000, "1984-85", Spouse_income = 50000), 366.66)
})

test_that("annual Medicare thresholds use their enacted income year", {
  expect_equal(medicare_levy(27000, "2024-25"), 0)
  expect_equal(medicare_levy(42000, "2024-25", sapto.eligible = TRUE), 0)
  expect_equal(medicare_levy(19500, "2010-11"), 66.1)
  expect_equal(medicare_levy(36000, "2015-16", is_married = TRUE), 0)
  expect_equal(medicare_levy(22000, "2016-17"), 34.5)
  expect_equal(medicare_levy(14000, "1999-00"), 90)
  expect_equal(medicare_levy(14000, "2000-01"), 38.6)
})

test_that("temporary budget repair levy taxes only the excess over $180000", {
  expect_equal(income_tax(c(179999, 180000, 180001, 200000), "2014-15"),
               c(58146.61, 58147, 58147.49, 67947))
  expect_equal(income_tax(200000, c("2014-15", "2015-16", "2016-17", "2017-18")),
               c(67947, 67947, 67632, 67232))
})

test_that("flood levy applies separately in 2011-12", {
  expect_equal(income_tax(c(50000, 60000, 100000, 150000), "2011-12"),
               c(8600, 12200, 26700, 46450))
  expect_equal(income_tax(60000, "2010-11"), 12150)
  # A large non-refundable tax offset can exhaust ordinary tax, but cannot
  # extinguish the flood levy ($50) or Medicare levy ($900).
  sys <- System(2012L, Offsets = set_offsets(set_offset(100000, 0L, 0)))
  expect_equal(income_tax(60000, "2011-12", System = sys), 950)
})
