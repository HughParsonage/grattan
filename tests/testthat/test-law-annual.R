test_that("historical temporary levies apply in the correct years and bands", {
  # Flood levy: Act 2011 No.15, s12B; repair levy: Act 2014 No.45, s34.
  expect_equal(income_tax(100000, "2010-11", age = 42), 26450)
  expect_equal(income_tax(100000, "2011-12", age = 42), 26700)
  expect_equal(income_tax(100000, "2011-12", age = 42,
                          System = System(2012L)), 26700)
  for (fy in c("2014-15", "2015-16", "2016-17")) {
    expect_equal(diff(income_tax(c(180000, 180001), fy, age = 42)), 0.49)
  }
  expect_equal(income_tax(200000, "2014-15", age = 42), 67947)
  expect_equal(income_tax(200000, "2016-17", age = 42), 67632)
})

test_that("enacted ordinary rate reductions are selected by financial year", {
  # Income Tax Rates Act 1986, Schedule7 (1July2026 compilation).
  for (yr in 2025:2028) {
    expect_equal(income_tax(200000, yr2fy(yr), age = 42),
                 c(60138, 60138, 59870, 59602)[yr - 2024L])
  }
  expect_error(income_tax(50000, "1982-83"), "1984.*2030")
  expect_error(System(2031L), "unsupported")
})

test_that("early Medicare phase-in rates and ceilings follow the annual Acts", {
  # Medicare Levy Act1983 ss6-8,1984 ss6-8,1986 ss6-7; ATO Taxpack1986.
  expect_equal(medicare_levy(7000, "1983-84"), 25.1264)
  expect_equal(medicare_levy(7200, "1984-85"), 18)
  expect_equal(medicare_levy(7600, "1985-86"), 14.8)
  expect_equal(medicare_levy(8500, "1986-87"), 94)
  expect_equal(medicare_levy(50000, "1983-84"), 208)
  expect_equal(medicare_levy(50000, "1984-85"), 500)
  expect_equal(medicare_levy(50000, "1985-86"), 500)
  expect_equal(medicare_levy(50000, "1986-87"), 572.5)
  expect_equal(medicare_levy(100000, "1983-84"), 291.20)
  expect_equal(medicare_levy(100000, "1984-85"), 733.32)
  expect_equal(medicare_levy(100000, "1985-86"), 1000)
  expect_equal(income_tax(100000, "1983-84", age = 70,
                          System = System(1984L)),
               income_tax(100000, "1983-84", age = 42))
})

test_that("Medicare annual thresholds use zero-liability endpoints", {
  # Act2000No54, Act2002No39, Act2011No44, Act2016No39, Act2017No58.
  expect_equal(medicare_levy(14000, "1999-00"), 90)
  expect_equal(medicare_levy(14000, "2000-01"), 38.6)
  expect_equal(medicare_levy(15000, "2001-02"), 92.2)
  expect_equal(medicare_levy(19500, "2010-11"), 66.1)
  expect_equal(medicare_levy(36000, "2015-16", is_married = TRUE), 0)
  expect_equal(medicare_levy(22000, "2016-17"), 34.5)
  expect_equal(income_tax(60000, "1999-00", age = 70), 19702)
})

test_that("latest enacted Medicare thresholds are used", {
  # Medicare Levy Act1986 compilations28March2025 and1July2026, ss3/8.
  expect_equal(medicare_levy(27000, "2024-25"), 0)
  expect_equal(medicare_levy(42000, "2024-25", sapto.eligible = TRUE), 0)
  expect_equal(medicare_levy(28000, "2025-26"), 0)
  expect_equal(medicare_levy(44000, "2025-26", sapto.eligible = TRUE), 0)
  expect_equal(medicare_levy(29000, "2025-26"), 98.9)
})

test_that("SATO uses senior schedules and year-specific shading thresholds", {
  # Retirees Taxpack2003 TableA; Treasury Budget2008-09; ATO2010instructions.
  expect_equal(lito(20000, "2002-03"), 150)
  expect_equal(income_tax(20000, "2002-03", age = 42), 2530)
  expect_equal(sapto(20000, "2002-03"), 2230)
  expect_equal(sapto(32005, "2011-12"), 2065)
  thresholds <- c(20000, 20500, 21968, 24867, 25867, 28867, 29867, 30685)
  years <- c(2003L, 2004L, 2006L, 2007L, 2008L, 2009L, 2010L, 2011L)
  for (i in seq_along(years)) {
    expect_equal(sapto(thresholds[i] + 88, yr2fy(years[i])), 2219)
  }
  # ATO2025instructions and enacted15%/14% ordinary rates (reg9 formula).
  expect_equal(sapto(35007, "2024-25"), 2219)
  expect_equal(sapto(36122, "2026-27"), 2219)
  expect_equal(sapto(37396, "2027-28"), 2219)
})
