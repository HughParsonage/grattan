context("Inflators return correct results")

test_that("cpi returns known results", {
  expect_gt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2013-14", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
            1.029)
  expect_lt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2013-14", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
            1.030)
})

test_that("cpi_inflator_general_date same as cpi_inflator", {
  expect_equal(cpi_inflator(from_fy = "2013-14", to_fy = "2014-15", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2013-14", to_date = "2014-15", adjustment = "none", useABSConnection = FALSE))
  
  expect_equal(cpi_inflator(from_fy = "2010-11", to_fy = "2014-15", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2010-11", to_date = "2014-15", adjustment = "none", useABSConnection = FALSE))
  
  
})

test_that("cpi_inflator_general_date same as cpi_inflator when diverged", {
  expect_equal(cpi_inflator(from_fy = "2013-14", to_fy = "2014-15", adjustment = "none", useABSConnection = FALSE, allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2014-01-01", to_date = "2015-01-01", adjustment = "none", useABSConnection = FALSE))
})

test_that("cpi_inflator_general_date messages", {
  expect_message(cpi_inflator_general_date(from_date = "2013", to_date = "2014"))
  expect_error(cpi_inflator_general_date(from_date = "2015-Q5", to_date = "2016-Q5"))
})

test_that("cpi returns reasonable forecasts", {
  expect_gt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2015-16", adjustment = "none", useABSConnection = FALSE, allow.projection = TRUE), 
            1.05)
  expect_lt(cpi_inflator(from_nominal_price = 1, from_fy = "2012-13", to_fy = "2015-16", adjustment = "none", useABSConnection = FALSE, allow.projection = TRUE), 
            1.06)
})

test_that("ABS connection", {
  expect_equal(cpi_inflator(from_fy = "2010-11", to_fy = "2011-12", adjustment = "none"), 
               cpi_inflator(from_fy = "2010-11", to_fy = "2011-12", useABSConnection = TRUE, adjustment = "none"))
})