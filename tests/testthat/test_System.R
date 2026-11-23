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

test_that("ordinary single Medicare thresholds survive System round trips", {
  system <- System(2018L, medicare_levy_lower_threshold = 30000L, fix = 2L)
  expect_identical(system$Medicare$lower_single_threshold, 30000L)
  expect_identical(system$Medicare$upper_single_threshold, 37500L)
  roundtrip <- .validateSystem(system)
  expect_identical(roundtrip$Medicare, system$Medicare)
  income <- c(25000L, 29000L, 34000L)
  # Ordinary tax less LITO, plus $400 of Medicare at $34,000.
  expected <- c(847, 1607, 2957)
  expect_equal(income_tax(income, "2017-18", System = roundtrip), expected)
  flat <- list(medicare_levy_lower_threshold = 30000L, medicare_levy_upper_threshold = 37500L)
  expect_equal(income_tax(income, "2017-18", System = flat), expected)
  sample <- data.table::data.table(Taxable_Income = income)
  expect_equal(model_income_tax(sample, "2017-18", return. = "tax",
                                medicare_levy_lower_threshold = 30000L,
                                medicare_levy_upper_threshold = 37500L), expected)
})

test_that("custom systems enforce the supported offset count", {
  limit <- the_MAX_N_OFFSETN()
  offsets <- rep(list(set_offset(1L, 0L, 0)), limit)
  system <- System(2018L, Offsets = offsets)
  expect_length(system$Offsets, limit)
  expect_equal(income_tax(50000L, "2017-18", System = system), 7797 + 1000 - limit)
  expect_error(System(2018L, Offsets = c(offsets, offsets[1L])), "at most.*offsets")
})

test_that("zero SAPTO taper keeps all eligible categories flat", {
  system <- System(2018L, sapto_taper = 0, fix = 1L)
  cutoffs <- c("upr_single", "upr_couple", "upr_illness")
  expect_equal(unname(unlist(system$Sapto[cutoffs])), rep(Inf, 3))
  expect_identical(.validateSystem(system)$Sapto, system$Sapto)
  expect_equal(income_tax(30000L, "2017-18", age = 70, System = system), 0)
  expect_equal(income_tax(30000L, "2017-18", age = 70, System = list(sapto_taper = 0)), 0)
  # A spouse with no income leaves the recipient's flat amount intact.
  # Two large taxable incomes can together exceed the integer range.
  income <- c(100000L, rep(.Machine$integer.max, 3L))
  claims <- data.frame(on_sapto_cd = c("D", "A", "B", "D"),
                       sp_taxable_income_loss = c(0L, 0L, rep(.Machine$integer.max, 2L)))
  with_rebate <- income_tax(income, "2017-18", .dots.ATO = claims, System = system)
  claims$on_sapto_cd <- ""
  expect_equal(income_tax(income, "2017-18", .dots.ATO = claims, System = system) - with_rebate,
               c(1602, 2230, 2040, 1602))
})
