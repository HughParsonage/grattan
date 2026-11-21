test_that("small-business offset reaches public tax and summary paths", {
  d <- data.table::data.table(Taxable_Income = c(50000L, 50000L),
    c_age_30_june = 42L, sp_flag = FALSE, Total_PP_BI_amt = 0L,
    Total_PP_BE_amt = 0L, Total_NPP_BI_amt = 100000L,
    Total_NPP_BE_amt = 50000L, Tot_net_small_business_inc = 50000L)
  years <- c("2018-19", "2021-22")
  # 2018-19: basic7797 - LITO250 - LMITO1080 - SBITO623.76 + levy1000.
  # 2021-22: basic6717 - LITO250 - LMITO1500 - cappedSBITO1000 + levy1000.
  expected <- c(6843.24, 4967)
  expect_equal(income_tax(d$Taxable_Income, years, .dots.ATO = d), expected)
  expect_equal(income_tax(d$Taxable_Income, years, .dots.ATO = d, return.mode = "sum"), sum(expected))
  expect_equal(income_tax(d$Taxable_Income, years, .dots.ATO = d, return.mode = "mean"), mean(expected))
  expect_equal(income_tax(d$Taxable_Income, "2018-19", .dots.ATO = d, return.mode = "sum"),
               2 * expected[1])
  expect_equal(model_income_tax(data.table::copy(d[1]), "2018-19", return. = "tax"), expected[1])
})

test_that("business offsets use custom ordinary tax and cannot extinguish levies", {
  d <- data.table::data.table(Taxable_Income = 50000L, c_age_30_june = 42L,
    sp_flag = FALSE, Total_PP_BI_amt = 0L, Total_PP_BE_amt = 0L,
    Total_NPP_BI_amt = 100000L, Total_NPP_BE_amt = 50000L,
    Tot_net_small_business_inc = 50000L)
  custom <- System(2019L, ordinary_tax_thresholds = c(0L, 18200L),
                    ordinary_tax_rates = c(0, 0.2))
  expect_equal(income_tax(50000, "2018-19", .dots.ATO = d, System = custom),
               6360 - 250 - 1080 - 6360 * 0.08 + 1000)
  custom <- System(2019L, Offsets = set_offsets(
    set_offset(100000L, thresholds = 0L, tapers = 0)))
  expect_equal(income_tax(50000, "2018-19", .dots.ATO = d, System = custom), 1000)
  custom <- System(2019L, Offsets = set_offsets(
    set_offset(100000L, thresholds = 0L, tapers = 0, refundable = TRUE)))
  expect_equal(income_tax(50000, "2018-19", .dots.ATO = d, System = custom),
               7797 - 7797 * 0.08 - 100000 + 1000)
  d[, Tot_net_small_business_inc := 0L]
  expect_equal(income_tax(50000, "2018-19", .dots.ATO = d, System = custom),
               7797 - 100000 + 1000)
})

test_that("mixed-year assessments preserve record order and summary arithmetic", {
  income <- c(200000, 100000, 200000, 100000)
  years <- c("2026-27", "2010-11", "2014-15", "2011-12")
  expected <- c(59870, 26450, 67947, 26700)
  expect_equal(income_tax(income, years), expected)
  expect_equal(income_tax(income, years, return.mode = "sum"), sum(expected))
  expect_equal(income_tax(income, years, return.mode = "mean"), mean(expected))
})
