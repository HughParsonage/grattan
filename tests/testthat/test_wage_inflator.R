context("Wage inflator")

test_that("Error handling", {
  expect_error(wage_inflator(from_fy = NA, to_fy = "2015-16"))
  
  expect_error(wage_inflator(1, from_fy = "2013-14", to_fy = "2045-46", allow.projection = FALSE), regexp = "wage index data")
})

test_that("upper/lower higher/lower", {
  expect_gte(wage_inflator(from_fy = "2013-14", to_fy = "2030-31", forecast.series = "upper"), 
             wage_inflator(from_fy = "2013-14", to_fy = "2030-31", forecast.series = "lower"))
})

test_that("ABS connection", {
  x <- wage_inflator(1, from_fy = "2001-02", to_fy = "2002-03")
  y <- wage_inflator(1, from_fy = "2001-02", to_fy = "2002-03", useABSConnection = TRUE)
  expect_equal(x, 
               y)
})

test_that("Custom wage series", {
  x <- wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
                     forecast.series = "custom", wage.series = data.table(fy_year = c("2015-16", "2016-17", "2017-18"), 
                                                                          r = c(0, 0, 0.10)))
  expect_equal(x, 1.1)
  
  expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
               forecast.series = "custom", 
               wage.series = data.table(fy_year = c("2016-17", "2017-18"), 
                                        r = c(0, 0.1))))
  
  expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
               forecast.series = "custom", 
               wage.series = data.table(fy_year = c("2015-16", "2016-17", "2017-18"), 
                                        r = c(42, 0.1, 0.1))))
  
  expect_message(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
                               forecast.series = "custom", 
                               wage.series = data.table(fy_year = c("2015-16", "2016-17", "2017-18"), 
                                                        r = c(0, 2.5, 10.0))))
})

