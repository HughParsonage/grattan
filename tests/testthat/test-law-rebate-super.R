test_that("SAPTO rebate income adds losses and reportable super only", {
  person <- function(...) data.table::data.table(
    c_age_30_june = 70L, sp_flag = FALSE, on_sapto_cd = "A", ...)
  tax <- function(d) income_tax(35000, "2023-24", .dots.ATO = d)
  # Ordinary tax 3192, LITO 700, SAPTO 2230 - .125 * (rebate - 32279).
  expect_equal(tax(person()), 602)
  expect_equal(tax(person(Net_rent_amt = -5000L)), 1227)
  expect_equal(tax(person(Net_rent_amt = 5000L)), 602)
  expect_equal(tax(person(MCS_Emplr_Contr = 5000L)), 602)
  for (amount in list(5000, 5000L)) {
    expect_equal(tax(person(Net_fincl_invstmt_lss_amt = amount)), 1227)
    expect_equal(tax(person(it_invest_loss = amount)), 1227)
    expect_equal(tax(person(Rptbl_Empr_spr_cont_amt = amount)), 1227)
    expect_equal(tax(person(Non_emp_spr_amt = amount)), 1227)
    expect_equal(tax(person(ic_rebate_income = 40000 + 0 * amount)), 1227)
  }
  expect_equal(tax(person(MCS_Emplr_Contr = 10000L,
                         Rptbl_Empr_spr_cont_amt = 5000L)), 1227)
})

test_that("rebate fringe adjustment uses the income year's FBT rate", {
  # ATO annual rebate worksheets: .535, .53, .51, .51, .53.
  years <- c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18")
  expect_equal(rebate_income(rep(35000, 5), Rep_frng_ben_amt = 5000,
                            fy.year = years),
               c(37675, 37650, 37550, 37550, 37650))
  d <- data.table::data.table(c_age_30_june = 70L, sp_flag = FALSE,
                              on_sapto_cd = "A", Rep_frng_ben_amt = 5000)
  # 3192 - LITO445 - SAPTO1572 + Medicare126.20.
  expect_equal(income_tax(35000, "2015-16", .dots.ATO = d), 1301.20)
  d[, Rep_frng_ben_amt := NA_real_]
  expect_equal(income_tax(35000, "2015-16", .dots.ATO = d),
               income_tax(35000, "2015-16", age = 70))
})

test_that("compact numeric addbacks give the same legal result across threads", {
  losses <- seq_len(1000)
  records <- data.frame(c_age_30_june = 70L, sp_flag = FALSE,
                         on_sapto_cd = "A", it_invest_loss = losses)
  expected <- 3192 - 700 - ceiling(2230 - (35000 + losses - 32279) / 8)
  expect_equal(income_tax(rep(35000L, 1000), "2023-24",
                          .dots.ATO = records, nThread = 1L), expected)
  expect_equal(income_tax(rep(35000L, 1000), "2023-24",
                          .dots.ATO = records, nThread = 2L), expected)
})

test_that("Division 293 excludes excess contributions exactly once", {
  d <- data.table::data.table(
    MCS_Emplr_Contr = c(20000, 30000, 40000, 70000),
    MCS_Prsnl_Contr = 0, MCS_Othr_Contr = 0, MCS_Ttl_Acnt_Bal = 0,
    Non_emp_spr_amt = 0, Net_rent_amt = 0, Net_fincl_invstmt_lss_amt = 0,
    Rep_frng_ben_amt = 0, Taxable_Income = 310000,
    Rptbl_Empr_spr_cont_amt = 0)
  a <- apply_super_caps_and_div293(d, cap = 30000, age_based_cap = FALSE,
                                  div293_threshold = 300000)
  expect_equal(a$low_tax_contributions_div293, c(20000, 30000, 30000, 30000))
  expect_equal(a$div293_tax, c(3000, 4500, 4500, 4500))
  expect_equal(a$Taxable_income_for_ECT, c(310000, 310000, 320000, 350000))
  d <- d[1:2]
  d[, Taxable_Income := 280000]
  a <- apply_super_caps_and_div293(d, cap = 30000, age_based_cap = FALSE,
                                  div293_threshold = 300000)
  expect_equal(a$div293_tax, c(0, 1500))
})

test_that("super output names and types survive dropping helpers by reference", {
  d <- data.table::data.table(Taxable_Income = 249000L, MCS_Emplr_Contr = c(30000L, 35000L),
                              concessional_cap = 0L, div293_income = 0L, rental_losses = 0L)
  apply_super_caps_and_div293(d, cap = 30000.5, age_based_cap = FALSE, div293_threshold = 250000,
                              copyDT = FALSE, drop_helpers = TRUE, colname_div293_tax = "div293_income",
                              warn_if_colnames_overwritten = FALSE)
  expect_identical(d$concessional_cap, c(30000.5, 30000.5))
  expect_equal(d$div293_income, 0.15 * c(29000, 30000.5))
  expect_null(d$rental_losses)
  missing <- data.table::data.table(Taxable_Income = c(NA_real_, NaN), MCS_Emplr_Contr = 30000)
  expect_identical(apply_super_caps_and_div293(missing, cap = 30000, age_based_cap = FALSE)$div293_tax,
                   c(NA_real_, NaN))
})

test_that("beneficiary offsets preserve interleaved years and custom rates", {
  expect_equal(bto(rep(6300.8, 3), c("2004-05", "2024-25", "2004-05")), c(51, 45, 51))
  expect_identical(bto(c(6300, 6300, Inf, NaN), rate1 = c(.17, .123, .15, .15)),
                   c(51, 37, Inf, NaN))
})

test_that("historical pension ages agree for dates and financial years", {
  years <- c("1995-96", "2003-04", "2011-12", "2023-24")
  for (sex in c("male", "m", "M", "female", "f", "F")) {
    expect_equal(age_pension_age(years, sex), age_pension_age(fy2date(years), sex))
  }
  expect_equal(age_pension_age(years, "male"), c(65, 65, 65, 67))
  expect_equal(age_pension_age(years, "female"), c(60.5, 62.5, 64.5, 67))
  expect_equal(age_pension_age(rep("1995-96", 2), c("male", "female")), c(65, 60.5))
})

test_that("inferred pension ages advance in the legislated half-year steps", {
  people <- data.frame(c_age_30_june = c(65L, 66L, 66L, 67L), sp_flag = FALSE)
  expect_equal(sf2osc(people, c(2018L, 2018L, 2022L, 2022L)),
               as.raw(c(0, 1, 0, 1)))
  expect_equal(sf2osc(people[1:2, ], 2020L), as.raw(c(0, 1)))
})

test_that("logical scalar inputs are recycled before native indexing", {
  expect_identical(do_rN(FALSE, integer(4)), integer(4))
  expect_identical(do_rN(TRUE, integer(4)), rep(1L, 4))
  expect_identical(do_rN(NA, integer(4)), integer(4))
})
