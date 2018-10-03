context("Inflators return correct results")

test_that("cpi returns known results", {
  expect_gt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2013-14", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
            1.029)
  expect_lt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2013-14", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
            1.030)
})

test_that("cpi returns reasonable forecasts", {
  expect_gt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2015-16", adjustment = "none", useABSConnection = FALSE, allow.projection = TRUE), 
            1.05)
  expect_lt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2015-16", adjustment = "none", useABSConnection = FALSE, allow.projection = TRUE), 
            1.06)
})
