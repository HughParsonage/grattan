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
  expect_equal(lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01"), 1.00124729250057, tol = 0.001)
})

test_that("lf_inflator returns long", {
  expect_equal(round(lf_inflator_fy(labour_force = c(1, 2), from_fy = "2010-11", to_fy = "2012-13"), 3),
               round(c(1.02691290641353, 2.05382581282705), 3), 
               tol = 0.002)
  
  expect_equal(lf_inflator(from_date = c("1981-01-01", "1981-02-01"), to_date = c("1981-02-01", "1981-01-01")), 
               c(1.00124729250057, 0.998754261299966), tol = 0.001)
})

test_that("lf_inflator_fy accepts multiple dates", {
  length_of <- length(lf_inflator_fy(labour_force = c(1, 2), from_fy = c("2010-11", "2012-13"), to_fy = c("2012-13", "2013-14")))
  
  expect_equal(length_of, 2)
})

test_that("ABS connection", {
  expect_equal(lf_inflator(from_date = c("1981-01-01", "1981-02-01"), to_date = c("1981-02-01", "1981-01-01"), useABSConnection = TRUE), 
               c(1.00124729250057, 0.998754261299966), tol = 0.001)
  
  expect_equal(lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01"), 
               lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01", useABSConnection = TRUE), 
               tol = 0.001) 
               
})


test_that("Custom lf series", {
  x <- lf_inflator_fy(1, from_fy = "2017-18", to_fy = "2018-19", 
                      forecast.series = "custom",
                      lf.series = data.table(fy_year = c("2016-17", "2017-18", "2018-19"), 
                                             r = c(0, 0, 0.10)))
  expect_equal(x, 1.1)
  
  y <- lf_inflator_fy(1, from_fy = "2022-23", to_fy = "2024-25", 
                      forecast.series = "custom", lf.series = 0.10)
  
  expect_equal(y, 1.1^2)
})
