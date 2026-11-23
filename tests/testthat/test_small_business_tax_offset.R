context("Small business tax offset")

test_that("standalone business income requires turnover evidence", {
  offset <- function(...) small_business_tax_offset(100000, 25000,
    total_net_small_business_income = 50000, fy_year = "2017-18", ...)
  expect_error(offset(), "aggregated_turnover.*previous_aggregated_turnover")
  expect_equal(offset(aggregated_turnover = 1e6), 1000)
  expect_equal(offset(previous_aggregated_turnover = 1e6), 1000)
  expect_equal(offset(aggregated_turnover = 5e6), 0)
})

test_that("Example in explanatory memo", {
  expect_equal(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = "2015-16"),
               625)
  
  
})

test_that("Always zero for fy_year", {
  expect_equal(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = "2014-15"),
               0)
  expect_equal(small_business_tax_offset(1:100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = "2014-15"),
               double(100e3))
  expect_equal(small_business_tax_offset(1:100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         fy_year = rep("2014-15", times = 100e3)),
               double(100e3))
})

test_that("a single business return can use different annual turnover thresholds", {
  record <- data.frame(aggregated_turnover = 3e6, Net_PP_BI_amt = 25000L,
                       isn_sbi_net = NA_integer_, Tot_net_small_business_inc = NA_integer_)
  years <- c("2015-16", "2016-17")
  expect_equal(small_business_tax_offset(50000L, 7797, .dots.ATO = record, fy_year = years),
               c(0, 7797 * 0.08 * 0.5))
  expect_equal(small_business_tax_offset(NA, 7797, .dots.ATO = record, fy_year = years),
               c(NA_real_, NA_real_))
})

test_that("Error handling", {
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3),
               regexp = "`fy_year` and `tax_discount` are both NULL")
  
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3, 
                                         tax_discount = "0.05"),
               regexp = "tax_discount. was class 'character'")
  
  expect_error(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         aggregated_turnover = 1e6,
                                         total_net_small_business_income =  50e3,
                                         tax_discount = c(0.05, 0.08)),
               regexp = "Provide a single value or a value for every observation.")
  
  expect_warning(small_business_tax_offset(100e3,
                                           basic_income_tax_liability = 25e3,
                                           aggregated_turnover = 1e6,
                                           total_net_small_business_income =  50e3,
                                           fy_year = "2015-16",
                                           tax_discount = 0.05),
                 regexp = "fy_year.*tax_discount")
  
})

test_that("Warnings with dotsATO", {
  skip_on_cran()


  library(data.table)
  s1314 <- as.data.table(.sample_file_1314())
  expect_warning(s1314[, small_business_tax_offset(Taxable_Income,
                                                   0.3 * Taxable_Income, 
                                                   .dots.ATO = .SD,
                                                   fy_year = "2015-16",
                                                   aggregated_turnover = 1)],
                 regexp = "Both `.dots.ATO` and `aggregated_turnover` were provided.", 
                 fixed = TRUE)
  expect_warning(s1314[, small_business_tax_offset(Taxable_Income,
                                                   0.3 * Taxable_Income, 
                                                   .dots.ATO = .SD, 
                                                   fy_year = "2015-16",
                                                   total_net_small_business_income = 1)],
                 regexp = "Both `.dots.ATO` and `total_net_small_business_income` were provided.", 
                 fixed = TRUE)
  # Without a turnover or an eligible amount, a net profit alone is insufficient.
  s1314 <- s1314[, .(Taxable_Income, Net_NPP_BI_amt)]
  expect_error(s1314[, small_business_tax_offset(Taxable_Income, 
                                                 0.3 * Taxable_Income, 
                                                 fy_year = "2015-16",
                                                 .dots.ATO = .SD)], 
               regexp = "does not contain the necessary variables")
  
})

test_that("Results with .dots.ATO", {
  skip_on_cran()
  library(data.table)
  s1314 <- as.data.table(.sample_file_1314())
  sbto_s1314 <- 
    s1314 %>%
    .[order(Taxable_Income)] %>%
    .[, sbto := small_business_tax_offset(Taxable_Income, 
                                          income_tax(Taxable_Income, "2016-17", .dots.ATO = .SD), 
                                          .dots.ATO = .SD,
                                          fy_year = "2016-17")]
  # SBTO cannot be negative
  expect_gte(min(sbto_s1314[["sbto"]]), 0)
  expect_equal(sbto_s1314[Ind == 106934L, sbto], 1000)
  expect_equal(sbto_s1314[Ind == 48540L, as.integer(sbto)], 16L)
  
  dt <- data.table(Taxable_Income = 100e3, 
                   Total_PP_BE_amt = 1e6,
                   Total_PP_BI_amt = 0,
                   Total_NPP_BE_amt = 0,
                   Total_NPP_BI_amt = 0,
                   Tot_net_small_business_inc = 50e3)
  
  expect_equal(small_business_tax_offset(100e3,
                                         basic_income_tax_liability = 25e3,
                                         .dots.ATO = dt,
                                         fy_year = "2015-16"),
               625)
})


test_that("pre-commencement years require no turnover evidence",
          {
            years <- c("2013-14", "2014-15")
            expect_identical(small_business_tax_offset(c(5e4, 5e4), 1e4, fy_year = years), double(2))
            expect_identical(small_business_tax_offset(5e4, 1e4, fy_year = years), double(2))
            expect_error(small_business_tax_offset(5e4, 1e4, fy_year = c("2014-15", "2015-16")), "turnover")
            expect_error(small_business_tax_offset(1:3, 1e4, fy_year = years), "length")
          })
