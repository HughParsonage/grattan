test_that("Validation of System", {
  Sys <- System(2018L, 
                sapto_lower_threshold = 30e3, 
                sapto_max_offset = 2000)
  expect_equal(Sys$Sapto$max_offset, 2000)
  Sys <-
    System(2018L,
           medicare_levy_lower_family_sapto_threshold = 0L,
           medicare_levy_upper_family_sapto_threshold = 0L)
  expect_equal(Sys$yr, 2018L)
  expect_error(System(2018L,
                      medicare_levy_lower_family_sapto_threshold = 2L,
                      medicare_levy_upper_family_sapto_threshold = 1L),
               "medicare_levy_upper_family_sapto_threshold")
  expect_error(System(2018L,
                      medicare_levy_lower_family_sapto_threshold = 2L,
                      medicare_levy_upper_family_sapto_threshold = 1L),
               "medicare_levy_upper_family_sapto_threshold")
  expect_error(System(2018L,
                      ordinary_tax_thresholds = c(0L, 9999L),
                      ordinary_tax_rates = c(0, 0.2, 0.2)),
               "ordinary_tax")
  expect_warning(System(2018L,
                        medicare_levy_lower_threshold = 100e3L,
                        fix = 1L),
                 "medicare_levy_upper_threshold")
  expect_error(System(2018L,
                      Offsets = set_offsets(set_offset(thresholds = integer(5),
                                                       tapers = double(4)))),
               "length")
  expect_warning(System(2018L, sapto_taper = -0.5, fix = 1L),
                 "taper")
})

test_that("Misc functions", {
  expect_equal(head(grattan:::rate_by_year(2018L), 5), c(0, 0.19, 0.325, 0.37, 0.45))
  expect_equal(head(grattan:::brack_by_year(2018L), 5), c(0, 18200, 37000, 87000, 180000))
})

test_that("Historical tax tables match the TS23 workbook", {
  expect_equal(head(grattan:::brack_by_year(fy2yr("2009-10")), 5),
               c(0, 6000, 35000, 80000, 180000))
  expect_equal(head(grattan:::rate_by_year(fy2yr("2009-10")), 5),
               c(0, 0.15, 0.30, 0.38, 0.45))

  expect_equal(head(grattan:::brack_by_year(fy2yr("2010-11")), 5),
               c(0, 6000, 37000, 80000, 180000))
  expect_equal(head(grattan:::rate_by_year(fy2yr("2010-11")), 5),
               c(0, 0.15, 0.30, 0.37, 0.45))

  expect_equal(head(grattan:::brack_by_year(fy2yr("2020-21")), 5),
               c(0, 18200, 45000, 120000, 180000))
  expect_equal(head(grattan:::rate_by_year(fy2yr("2020-21")), 5),
               c(0, 0.19, 0.325, 0.37, 0.45))

  expect_equal(head(grattan:::brack_by_year(fy2yr("2021-22")), 5),
               c(0, 18200, 45000, 120000, 180000))
  expect_equal(head(grattan:::rate_by_year(fy2yr("2021-22")), 5),
               c(0, 0.19, 0.325, 0.37, 0.45))

  expect_equal(head(grattan:::brack_by_year(fy2yr("2022-23")), 5),
               c(0, 18200, 45000, 120000, 180000))
  expect_equal(head(grattan:::rate_by_year(fy2yr("2022-23")), 5),
               c(0, 0.19, 0.325, 0.37, 0.45))
})
