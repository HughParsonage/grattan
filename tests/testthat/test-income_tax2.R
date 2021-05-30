test_that("income_tax2 works", {
  expect_equal(income_tax2(180e3, "2017-18"), 57832)
  expect_equal(income_tax2(180e3, "2017-18", medicare_levy_rate = 0.01), 57832 - 0.01 * 180e3)
  expect_equal(income_tax2(38e3, "2018-19"),
               income_tax(38e3, "2018-19"))
})

test_that("dots ATO", {
  skip_on_cran()
  skip_if_not_installed("taxstats")
  skip_if_not_installed("data.table")
  library(data.table)
  
  s1314 <- 
    # ignore sbto for now
    # Ind 202215 seems strange
    copy(taxstats::sample_file_1314)[Total_NPP_BI_amt == 0][Ind != 202215L]
  
  for (fy_ in yr2fy(2014:2022)) {
    s1314[, tax0 := 0]
    s1314[, tax2 := 0]
    s1314[, tax0 := income_tax(Taxable_Income, fy_, .dots.ATO = s1314)]
    s1314[, tax2 := income_tax2(Taxable_Income, fy_, .dots.ATO = s1314)]
    s1314[, expect_equal(tax0, tax2, info = paste0("FY = ", fy_))]
  }
  
})
