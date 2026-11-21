test_that("the 2021-22 LMITO respects its statutory income cutoff", {
  # ITAA97 ss61-105/61-107 and 2022 Act14 Schedule6: the $420 addition
  # excludes exactly $126,000; higher incomes have no entitlement at all.
  income <- c(125999, 126000, 126001, 130000, 140000)
  expect_equal(lmito(income, "2021-22"), c(420.03, 0, 0, 0, 0))
  expect_equal(lmito(as.integer(income), "2021-22"), c(420.03, 0, 0, 0, 0))
  expect_equal(lmito(c(125999, 126000, 126001), "2020-21"), c(0.03, 0, 0))
  expect_equal(lmito(48000, "2022-23"), 0)

  offsets <- set_offsets(yr = 2022L)
  expect_equal(offsets[[2]]$cutoff, 126000L)
  expect_equal(nOffset_upper_threshold(offsets, 2L), 126000)
  system <- System(2022L, Offsets = offsets)
  expect_equal(system$Offsets[[2]]$cutoff, 126000L)
  expect_equal(income_tax(c(126000, 130000), "2021-22", age = 42,
                          System = system), c(34207, 35767))
  records <- data.table::data.table(Taxable_Income = c(126000L, 130000L),
                                    c_age_30_june = 42L, sp_flag = FALSE)
  expect_equal(model_income_tax(records, "2021-22", return. = "tax"),
               c(34207, 35767))
})

test_that("custom offset income limits survive serialization", {
  offset <- set_offset(500, 90000L, 0.03, cutoff = 100000L)
  expect_equal(multiOffset(c(99999L, 100000L, 100001L), list(offset)),
               c(200.03, 0, 0))
  expect_equal(nOffset_upper_threshold(list(offset)), 100000)
  legacy <- offset[names(offset) != "cutoff"]
  expect_equal(multiOffset(100000L, list(legacy)), 200)
  expect_error(set_offset(cutoff = -1L), "cutoff")
})

test_that("small-business offset rates follow the accelerated legislation", {
  years <- c("2014-15", "2015-16", "2016-17", "2019-20", "2020-21",
              "2021-22", "2024-25", "2026-27", "2027-28")
  basic <- c(7797, 7797, 7797, 7797, 6717, 6717, 5788, 5520, 5252)
  expect_equal(small_business_tax_offset(50000, basic,
                                         aggregated_turnover = 1000000,
                                         total_net_small_business_income = 25000,
                                         fy_year = years),
               c(0, 194.925, 311.88, 311.88, 436.605, 537.36, 463.04, 441.6, 420.16))
})

test_that("small-business eligibility and business share obey the statutory limits", {
  expect_equal(small_business_tax_offset(40000, 4547,
                                         aggregated_turnover = c(1999999, 2000000),
                                         total_net_small_business_income = 20000,
                                         fy_year = "2015-16"), c(113.675, 0))
  expect_equal(small_business_tax_offset(40000, 4547,
                                         aggregated_turnover = c(2000000, 4999999, 5000000),
                                         total_net_small_business_income = 20000,
                                         fy_year = "2017-18"), c(181.88, 181.88, 0))
  # Business income can exceed taxable income after non-business deductions.
  expect_equal(small_business_tax_offset(40000, 4547,
                                         aggregated_turnover = 100000,
                                         total_net_small_business_income = 60000,
                                         fy_year = "2015-16"), 227.35)
  expect_equal(small_business_tax_offset(0, 0, aggregated_turnover = 100000,
                                         total_net_small_business_income = 0,
                                         fy_year = "2021-22"), 0)
  expect_warning(value <- small_business_tax_offset(40000, 4547,
                                                     aggregated_turnover = 3000000,
                                                     total_net_small_business_income = 20000,
                                                     fy_year = "2017-18", tax_discount = 0.1),
                  "fy_year.*tax_discount")
  expect_equal(value, 227.35)
})

test_that("income_tax includes the small-business offset from supplied business data", {
  records <- data.table::data.table(Taxable_Income = 50000L, c_age_30_june = 42L,
                                    sp_flag = FALSE, Total_PP_BI_amt = 0L,
                                    Total_PP_BE_amt = 0L, Total_NPP_BI_amt = 100000L,
                                    Total_NPP_BE_amt = 50000L,
                                    Tot_net_small_business_inc = 50000L)
  # Ordinary7797 - LITO250 - LMITO1080 + Medicare1000 - SBITO623.76.
  expect_equal(income_tax(50000L, "2018-19", .dots.ATO = records), 6843.24)
})

test_that("large integer business turnover cannot overflow into eligibility", {
  records <- data.table::data.table(Taxable_Income = 50000L, c_age_30_june = 42L,
                                    sp_flag = FALSE, Total_PP_BI_amt = 1500000000L,
                                    Total_PP_BE_amt = 1499975000L,
                                    Total_NPP_BI_amt = 1500000000L,
                                    Total_NPP_BE_amt = 1499975000L,
                                    Tot_net_small_business_inc = 50000L)
  expect_equal(small_business_tax_offset(50000L, 7797, .dots.ATO = records,
                                         fy_year = "2018-19"), 0)
  expect_equal(income_tax(50000L, "2018-19", .dots.ATO = records), 7467)
  double_records <- data.table::copy(records)
  columns <- c("Total_PP_BI_amt", "Total_PP_BE_amt", "Total_NPP_BI_amt", "Total_NPP_BE_amt")
  for (column in columns) {
    data.table::set(double_records, j = column, value = as.double(double_records[[column]]))
  }
  expect_equal(income_tax(50000L, "2018-19", .dots.ATO = records),
               income_tax(50000L, "2018-19", .dots.ATO = double_records))
})

test_that("SAPTO uses year-specific senior thresholds", {
  expect_equal(sapto(35007, "2024-25"), 2219)
  expect_equal(sapto(c(34919, 52759), "2024-25"), c(2230, 0))
  expect_equal(sapto(20000, "2002-03"), 2230)
  expect_equal(sapto(32005, "2011-12"), 2065)
  expect_equal(lito(20000, "2002-03"), 150)
  expect_equal(income_tax(20000, "2002-03", age = 42), 2530)
})

test_that("SAPTO spouse transfers follow the statutory decreasing taper", {
  # Regulations9/11/12: base3204, recalculated threshold37227 after ceiling.
  expect_equal(sapto(c(37226, 37227, 40003, 62858, 62859, 70000), "2017-18",
                     Spouse_income = 0, family_status = "married", on_sapto_cd = "D"),
               c(3204, 3204, 2857, 0.125, 0, 0))
  # ATO2025-26 worked example: illness-separated couple, spouse unused1440,
  # increased base3480, rebate threshold39912 and own rebate income48000.
  expect_equal(sapto(48000, "2025-26", Spouse_income = 10000,
                     family_status = "separated_due_illness", on_sapto_cd = "B"), 2469)
  expect_equal(sapto(30000, "2017-18", Spouse_income = 60000,
                     family_status = "married", on_sapto_cd = "D"), 0)
  expect_equal(sapto(c(30000, 30000), "2017-18", Spouse_income = c(53579, 53580),
                     family_status = "married", on_sapto_cd = "D"), c(1473.75, 0))
  # Combined eligibility uses spouse rebate income, while the unused
  # transfer uses spouse taxable income (not reportable super/loss add-backs).
  records <- data.table::data.table(Taxable_Income = 40003L, c_age_30_june = 70L,
                                    sp_flag = TRUE, on_sapto_cd = "D",
                                    sp_taxable_income_loss = 0L,
                                    spc_rebate_income = 30000L)
  no_levy <- System(2018L, medicare_levy_rate = 0, fix = 2L)
  # Ordinary4547.975 - LITO399.955 - transferred SAPTO2857.
  expect_equal(income_tax(40003L, "2017-18", .dots.ATO = records, System = no_levy),
               1291.02)
})

test_that("SAPTO categories agree across scalar, vector and raw claim codes", {
  codes <- c("A", "B", "C", "D", "E")
  expected <- c(2230, 2040, 2040, 1602, 1602)
  expect_equal(sapto(rep(20000, 5), "2017-18", Spouse_income = 30000,
                     on_sapto_cd = codes), expected)
  scalar <- vapply(codes, function(code) sapto(20000, "2017-18", Spouse_income = 30000,
                                               on_sapto_cd = code), numeric(1))
  expect_equal(unname(scalar), expected)
  expect_equal(sapto(rep(20000, 5), "2017-18", Spouse_income = 30000,
                     on_sapto_cd = as.raw(1:5)), expected)
  expect_equal(sapto(20000, "2017-18", on_sapto_cd = ""), 0)
  expect_equal(sapto(20000, "2017-18", sapto.eligible = FALSE, fill = -1), -1)
  expect_equal(as_raw_sapto_cd(codes, nThread = 2L), as.raw(1:5))
  expect_equal(as_raw_sapto_cd(as.raw(1:5), nThread = 2L), as.raw(1:5))
  # Deferred character conversion is an ALTREP input; normalization must
  # access it only on the R thread even when multiple workers are requested.
  expect_equal(as_raw_sapto_cd(as.character(1:1000), nThread = 2L), raw(1000))
})

test_that("custom illness-separated SAPTO parameters survive System serialization", {
  system <- System(2024L, sapto_max_offset_illness = 3000,
                    sapto_lower_threshold_illness = 35000)
  expect_equal(system$Sapto$max_offset_illness, 3000)
  expect_equal(system$Sapto$lower_threshold_illness, 35000)
  expect_equal(system$Sapto$upr_illness, 118000)
  records <- data.table::data.table(Taxable_Income = 35000L, c_age_30_june = 70L,
                                    sp_flag = TRUE, on_sapto_cd = "B",
                                    sp_taxable_income_loss = 30000L,
                                    spc_rebate_income = 30000L)
  expect_equal(income_tax(35000L, "2023-24", .dots.ATO = records), 917.125)
  expect_equal(income_tax(35000L, "2023-24", .dots.ATO = records, System = system), 0)
})

test_that("SAPTO age inference uses the year and preserves explicit eligibility", {
  records <- data.frame(c_age_30_june = c(65L, 66L, 67L), sp_flag = c(FALSE, TRUE, TRUE))
  expect_equal(sf2osc(records, 2024L), as.raw(c(0, 0, 4)))
  expect_equal(sf2osc(data.frame(sp_flag = c(FALSE, TRUE)), c(2021L, 2024L)), as.raw(c(0, 0)))
  mixed <- data.frame(c_age_30_june = c(66L, 66L), sp_flag = FALSE)
  expect_equal(sf2osc(mixed, c(2021L, 2024L)), as.raw(c(1, 0)))
  custom <- System(2024L, sapto_pension_age = 70)
  expect_equal(sf2osc(data.frame(c_age_30_june = c(69L, 70L), sp_flag = FALSE),
                      2024L, custom), as.raw(c(0, 1)))
  younger <- data.table::data.table(Taxable_Income = 33000L, c_age_30_june = 42L,
                                    sp_flag = FALSE, on_sapto_cd = "A")
  expect_equal(income_tax(33000L, "2023-24", .dots.ATO = younger), 0)
})

test_that("beneficiary offsets use the year-specific band and prescribed rounding", {
  expect_equal(bto(c(40000, 45000, 50000), "2020-21"), c(5100, 5850, 7350))
  expect_equal(bto(c(6000.99, 25000.5), "2020-21"), c(0, 2850))
  expect_equal(bto(40000, c("2006-07", "2007-08", "2008-09", "2009-10", "2010-11")),
               c(7350, 6600, 6000, 5850, 5550))
  expect_equal(bto(40000, "2020-21", tax_threshold = 37000), 5550)
  expect_equal(bto(10000, "2020-21", rate1 = 0.1, benefit_threshold = 9000), 100)
})
