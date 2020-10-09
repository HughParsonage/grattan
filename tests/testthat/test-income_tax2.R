test_that("income_tax2 works", {
  expect_equal(income_tax2(180e3, "2017-18"), 57832)
  expect_equal(income_tax2(180e3, "2017-18", medicare_levy_rate = 0.01), 57832 - 0.01 * 180e3)
  expect_equal(income_tax2(38e3, "2018-19"),
               income_tax(38e3, "2018-19"))
})

test_that("dots ATO", {
  skip_if_not_installed("taxstats")
  skip_if_not_installed("data.table")
  library(data.table)
  s1314 <- copy(taxstats::sample_file_1314)
  s1314[, tax0 := income_tax(Taxable_Income, "2013-14", .dots.ATO = s1314)]
  s1314[, tax2 := income_tax2(Taxable_Income, "2013-14", .dots.ATO = s1314)]
  s1314[, expect_equal(tax0, tax2)]
  
})
