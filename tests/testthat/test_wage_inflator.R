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
                     forecast.series = "custom",
                     wage.series = data.table(fy_year = c("2016-17", "2017-18"), 
                                              r = c(0, 0.10)))
  expect_equal(x, 1.1)
  
  y <- wage_inflator(1, from_fy = "2015-16", to_fy = "2019-20", 
                     forecast.series = "custom", 
                     wage.series = 0.1)
  
  expect_equal(y, 1.1^4)
})
  
test_that("Custom wage series error handling", {
  expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
               forecast.series = "custom", 
               wage.series = data.table(fy_year = c("2015-16", "2016-17", "2017-18"), 
                                        r = c(42, 0.1, 0.1))))
  
  expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
                             forecast.series = "custom", 
                             wage.series = c(1, 2)))
  
  expect_message(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
                               forecast.series = "custom", 
                               wage.series = data.table(fy_year = c("2016-17", "2017-18"), 
                                                        r = c(2.5, 10.0))))
})

test_that("from > to deflates and is not a warning", {
  x <- wage_inflator(from_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                     to_fy = "2017-18")
  y <- wage_inflator(to_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                     from_fy = "2017-18")
  expect_equal(x, 1/y)
})

# Issue #24 in general:
test_that("from > to deflates and is not a warning for inflators", {
  x <- lf_inflator_fy(from_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                     to_fy = "2017-18")
  y <- lf_inflator_fy(to_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                     from_fy = "2017-18")
  expect_equal(x, 1/y)
  
  x <- cpi_inflator(from_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                    to_fy = "2017-18")
  y <- cpi_inflator(to_fy = c("2013-14", "2015-16", "2016-17", "2017-18"), 
                    from_fy = "2017-18")
  expect_equal(x, 1/y)
})

