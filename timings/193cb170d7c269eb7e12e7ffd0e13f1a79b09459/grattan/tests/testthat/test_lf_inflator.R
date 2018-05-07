context("lf inflator")

test_that("Error handling", {
  expect_error(lf_inflator_fy(from_fy = "2012-13", to_fy = "2099-00", allow.projection = FALSE), regexp = "to_fy are in labour force data")
})

test_that("upper and lower series produce higher and lower forecasts", {
  expect_gte(lf_inflator_fy(labour_force = 1, 
                            from_fy = "2012-13", 
                            to_fy = "2018-19", 
                            forecast.series = "upper"), 
             lf_inflator_fy(labour_force = 1, 
                            from_fy = "2012-13", 
                            to_fy = "2018-19", 
                            forecast.series = "mean"))
  expect_lte(lf_inflator_fy(labour_force = 1, 
                            from_fy = "2012-13", 
                            to_fy = "2018-19", 
                            forecast.series = "lower"), 
             lf_inflator_fy(labour_force = 1, 
                            from_fy = "2012-13", 
                            to_fy = "2018-19", 
                            forecast.series = "mean"))
})

test_that("lf_inflator returns known results", {
  expect_equal(lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01"), 1.00124729250057)
})

test_that("lf_inflator returns long", {
  expect_equal(lf_inflator_fy(labour_force = c(1, 2), from_fy = "2010-11", to_fy = "2012-13"), 
               c(1.02691290641353, 2.05382581282705))
  
  expect_equal(lf_inflator(from_date = c("1981-01-01", "1981-02-01"), to_date = c("1981-02-01", "1981-01-01")), 
               c(1.00124729250057, 0.998754261299966))
})

test_that("ABS connection", {
  expect_equal(lf_inflator(from_date = c("1981-01-01", "1981-02-01"), to_date = c("1981-02-01", "1981-01-01"), useABSConnection = TRUE), 
               c(1.00124729250057, 0.998754261299966))
  
  expect_equal(lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01"), 
               lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01", useABSConnection = TRUE)) 
               
})
