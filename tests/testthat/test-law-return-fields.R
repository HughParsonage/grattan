test_that("aLife and the sample supply different senior and family evidence", {
  # 2017-18 ordinary $3,193.33, LITO $445, SAPTO $1,889 and senior levy $24.90.
  senior <- 0.19 * (35007 - 18200) - 445 - 1889 + 24.9
  expect_equal(income_tax(35007, "2017-18", age = 65.75), senior)
  expect_equal(income_tax(35007, "2017-18", age = 70, .dots.ATO = data.frame(sp_flag = 0)), senior)
  alife <- data.frame(c_age_30_june = c(65L, 65L), c_pension_age_eligible = c(1L, 0L))
  expect_equal(sf2osc(alife, 2018L), as.raw(c(1, 0)))
  expect_equal(sf2osc(alife, 2018L, System(2018L, sapto_pension_age = 70)), raw(2))
  expect_equal(sf2osc(data.frame(Age_range = c(1L, 2L), Partner_status = c(1L, 0L)),
                      2018L), as.raw(c(4, 0)))
  expect_equal(sf2osc(data.frame(c_age_30_june = c(64, 64), c_gender = c(0L, 1L)),
                      2009L), as.raw(c(1, 0)))
  expect_equal(sf2osc(data.frame(c_age_30_june = c(64, 64), Gender = c(0L, 1L)),
                      2009L), as.raw(c(0, 1)))
  expect_equal(sf2osc(data.frame(on_sato_cd = c("R", "T", "C", "U", "K")), 1997L),
               as.raw(1:5))
  # The partnered claim establishes a family even where spouse taxable income is zero.
  married <- data.frame(on_sapto_cd = "E", sp_taxable_income_loss = 0L)
  expect_equal(income_tax(45000, "2024-25", .dots.ATO = married), 4288 - 325)
  expect_equal(income_tax(35000, "2017-18", .dots.ATO =
    data.frame(on_sapto_cd = "E", Spouse_taxable_income = 60000)), 3192 - 445 + 700)
})

test_that("historical and current senior rebate rules have their own boundaries", {
  expect_equal(sapto(c(11545, 11561, 21377), "1996-97"), c(614.5, 613.5, 0))
  expect_equal(sapto(c(11385, 11393), "1997-98", on_sapto_cd = "C"), c(1197,1196))
  expect_equal(sapto(11905, "1998-99"), 1301)
  expect_equal(sapto(c(12000, 23054), "1999-00"), c(1358, 0))
  expect_equal(sapto(20000, "2004-05", Spouse_income = 10000, on_sapto_cd = "D"),
               1602 + 1602 - 0.17 * 4000)
  expect_equal(sapto(25000, "2025-26", Spouse_income = 0, on_sapto_cd = "D"), 1602)
  # Zero spouse taxable income permits the full transfer despite $30,000 rebate income.
  expect_equal(sapto(40000, "2017-18", Spouse_income = 30000, spouse_taxable_income = 0,
                     on_sapto_cd = "D"), ceiling(3204 - (40000 - 37227) / 8))
  # $10,000 of a spouse's qualifying exempt pension changes the transfer
  # to 1602 - 0.15 * 4000. It leaves both partners' Medicare incomes intact.
  spouse <- data.frame(on_sapto_cd = "D", sp_taxable_income_loss = 0, sp_pension_exempt = 10000)
  no_levy <- System(2018L, medicare_levy_rate = 0, fix = 2L)
  expected <- sapto(40000, "2017-18", Spouse_income = 0, spouse_taxable_income = 10000, on_sapto_cd = "D")
  expect_equal(income_tax(40000, "2017-18", .dots.ATO = spouse, System = no_levy),
               4547 - 400 - expected)
  expect_equal(sapto(39000, "2025-26"), 1720)
  expect_equal(income_tax(52752, "2025-26", age = 70),
               4288 + .30 * 7752 - (325 - .015 * 7752) - 1 + .10 * (52752 - 44268))
  expect_equal(sapto(rep(35007, 2), c("2024-25", "2023-24")), c(2219, 1889))
  expect_equal(lito(rep(35000, 2), c("2019-20","2020-21")), c(445, 700))
  expect_equal(lmito(rep(125999, 2), c("2020-21","2021-22")), c(.03, 420.03))
  expect_equal(bto(6300, "2004-05"), 51)
  expect_identical(whole_dollar_tax(c(1000 - 1e-13, 999.9999, -999.9999)), c(1000L, 999L, -999L))
  expect_equal(medicare_levy(11746, "1990-91"), 0.20)
})

test_that("reported rental, fringe and FHSS inputs have dated treatments", {
  years <- c("2016-17", "2017-18", "2018-19")
  expect_equal(rebate_income(rep(35000L, 3), fy.year = years, exempt_fringe_benefits = 1000,
                             nonexempt_fringe_benefits = 4000, assessable_fhss = 1000),
               c(37550, 39530, 38530))
  expect_equal(rebate_income(35000L, fy.year = "2017-18", Rep_frng_ben_amt = 5000,
                             nonexempt_fringe_benefits = 4000), 39530)
  pairs <- data.frame(on_sapto_cd = "A", it_property_loss = c(5000L, NA),
                      Net_rent_amt = c(-2000, -5000), ic_rebate_income = c(NA, 35007))
  expect_equal(income_tax(35007, "2022-23", .dots.ATO = pairs),
               .19 * (35007 - 18200) - 700 - c(1264, 1889))
  pre <- data.frame(on_sato_cd = "A", ic_rebate_income = 37003, it_property_loss = 5000)
  expect_equal(income_tax(32003, "2008-09", .dots.ATO = pre), income_tax(32003, "2008-09", age = 70))
  # Spouse IT6 is already in sp_net_invest_loss, and can push the family over $83,580.
  expect_equal(income_tax(35000, "2017-18", .dots.ATO = data.frame(on_sapto_cd = "E",
    sp_taxable_income_loss = 45000, sp_net_invest_loss = 4000)), 3192 - 445 + 700)
})

test_that("qualifying return amounts and prior turnover reach the business offset", {
  offset <- function(d) small_business_tax_offset(80000, 17547, .dots.ATO = d, fy_year = "2018-19")
  eligible <- data.frame(isn_sbi_net = c(0, 10000), pt_share_sbi_pships = c(80000, 0),
                         pt_share_sbi_trusts = c(0, 10000), aggregated_turnover = 5100000)
  expect_equal(offset(eligible), c(1000, 17547 * .08 / 4))
  sampled <- data.frame(Total_PP_BI_amt = 0, Total_NPP_BI_amt = 5100000,
                        Net_PP_BI_amt = 0, Net_NPP_BI_amt = 80000, Net_PT_NPP_dsn = 500000)
  expect_equal(offset(sampled), 0)
  sampled$previous_aggregated_turnover <- 4900000
  expect_equal(offset(sampled), 1000)
  expect_equal(small_business_tax_offset(80000, 17547, aggregated_turnover = 5100000,
    previous_aggregated_turnover = 4900000, total_net_small_business_income = 80000,
    fy_year = "2018-19"), 1000)
  sampled$isn_sbi_net <- 0
  expect_equal(offset(sampled), 0)
  sampled$Tot_net_small_business_inc <- 80000
  expect_equal(offset(sampled), 1000)
})

test_that("the working Australians offset uses net labour income", {
  ordinary <- income_tax(60000, "2027-28")
  sample <- data.frame(Sw_amt = c(19700, 19700, 0, 0), WRE_other_amt = c(0, 1300, 0, 0),
                       Net_NPP_BI_amt = c(0, 0, 19700, 0), Net_PT_NPP_dsn = 90000)
  expect_equal(ordinary - income_tax(60000, "2027-28", .dots.ATO = sample), c(70, 28, 210, 0))
  alife <- data.frame(i_salary_wage = c(0, 19700, 101000), i_attributed_psi = c(18800, 0, 0),
                      c_resident = c("Y", "Y", "N"), net_labour_income = c(NA, 19000, NA))
  expect_equal(ordinary - income_tax(60000, "2027-28", .dots.ATO = alife), c(84, 112, 0))
  years <- c("2026-27", "2027-28", "2028-29", "2029-30")
  wages <- data.frame(Sw_amt = 101000)
  expect_equal(income_tax(100000, years) - income_tax(100000, years, .dots.ATO = wages),
               c(0, 250, 250, 250))
})

test_that("flood exemptions and reform controls reach the public assessment", {
  expect_equal(income_tax(100000, "2011-12") - income_tax(100000, "2011-12",
    .dots.ATO = data.frame(tn_flood_levy_exempt = c(1L, 0L))), c(250, 0))
  claimant <- data.table::data.table(on_sapto_cd = "A", c_age_30_june = 70L, Taxable_Income = 35007L)
  expect_equal(model_income_tax(claimant, "2023-24", return. = "tax", sapto_eligible = FALSE),
               .19 * (35007 - 18200) - 700 + .02 * 35007)
  expect_equal(claimant$c_age_30_june, 70L)
  business <- data.table::data.table(Taxable_Income = 50000L, isn_sbi_net = 50000L)
  expect_equal(model_income_tax(business, "2018-19", return. = "tax", sbto_discount = 0),
               7797 - 250 - 1080 + 1000)
})

test_that("Division 293 replaces the assessed excess and honours person caps", {
  alife <- data.table::data.table(ic_taxable_income_loss = c(230000, 220000),
    sc_empl_cont = 30000, sc_other_concess_cont = 10000, sc_other_cont = 10000,
    i_excess_super = c(10000, 0), sn_concess_cont_cap = c(30000, 40000))
  a <- apply_super_caps_and_div293(alife, div293_threshold = 250000)
  expect_equal(a$Taxable_income_for_ECT, c(230000, 220000))
  expect_equal(a$low_tax_contributions_div293, c(30000, 40000))
  expect_equal(a$div293_tax, c(1500, 1500))
  b <- apply_super_caps_and_div293(alife, age_based_cap = FALSE, cap = 40000,
                                   div293_threshold = 250000)
  expect_equal(b$Taxable_income_for_ECT, c(220000, 220000))
  expect_equal(b$div293_tax, c(1500, 1500))
  for (prefix in c("Spr_", "MCS_")) {
    d <- data.table::data.table(Taxable_Income = 220000, Age_range = c(1L, 7L))
    data.table::set(d, j = paste0(prefix, "Emplr_Contr"), value = 30000)
    data.table::set(d, j = paste0(prefix, "Othr_Contr"), value = 10000)
    a <- apply_super_caps_and_div293(d, cap = 30000, cap2 = 35000, cap2_age = 59,
                                    div293_threshold = 250000, use_other_contr = TRUE)
    expect_equal(a$excess_concessional_contributions, c(5000, 10000))
    expect_equal(a$div293_tax, c(1500, 1500))
  }
})
