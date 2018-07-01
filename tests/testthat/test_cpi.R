context("Inflators return correct results")

test_that("Default from_fy and to_fy", {
  expect_warning(cpi_inflator(), 
                 regexp = "`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  expect_equal(suppressWarnings(cpi_inflator()),
               cpi_inflator(from_fy = prev_fy(date2fy(Sys.Date())),
                              to_fy = date2fy(Sys.Date())))
})

test_that("Errors", {
  expect_error(cpi_inflator(to_fy = "2013-14"), 
               regexp = "`from_fy` is missing", 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14"), 
               regexp = "`to_fy` is missing",
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14", to_fy = "2040-41",
                            allow.projection = FALSE), 
               regexp = "`to_fy = 2040-41` yet `allow.projection = FALSE`.", 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14", to_fy = c("2015-16", "2040-41"),
                            allow.projection = FALSE), 
               regexp = "`to_fy` contains 2040-41, yet `allow.projection = FALSE`.", 
               fixed = TRUE)
  expect_error(cpi_inflator_quarters(from_qtr = "2015-Q1", to_qtr = "2015-01-01"), 
               regexp = "Dates must be in quarters.", 
               fixed = TRUE)
})

test_that("cpi returns known results", {
  expect_gt(cpi_inflator(from_nominal_price = 1,
                         from_fy = "2012-13",
                         to_fy = "2013-14",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = FALSE), 
            1.029)
  expect_lt(cpi_inflator(from_nominal_price = 1,
                         from_fy = "2012-13",
                         to_fy = "2013-14",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = FALSE), 
            1.030)
})

test_that("cpi_inflator_general_date same as cpi_inflator", {
  expect_equal(cpi_inflator(from_fy = "2013-14",
                            to_fy = "2014-15",
                            adjustment = "none",
                            useABSConnection = FALSE,
                            allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2013-14",
                                         to_date = "2014-15",
                                         adjustment = "none",
                                         useABSConnection = FALSE))
  
  expect_equal(cpi_inflator(from_fy = "2010-11",
                            to_fy = "2014-15",
                            adjustment = "none",
                            useABSConnection = FALSE,
                            allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2010-11",
                                         to_date = "2014-15",
                                         adjustment = "none",
                                         useABSConnection = FALSE))
  
  
})

test_that("cpi_inflator_general_date same as cpi_inflator when diverged", {
  expect_equal(cpi_inflator(from_fy = "2013-14",
                            to_fy = "2014-15",
                            adjustment = "none",
                            useABSConnection = FALSE,
                            allow.projection = FALSE), 
               cpi_inflator_general_date(from_date = "2014-01-01",
                                         to_date = "2015-01-01",
                                         adjustment = "none",
                                         useABSConnection = FALSE))
  expect_equal(cpi_inflator_general_date(from_date = "2014-Q1", 
                                         to_date = "2015-Q1", 
                                         useABSConnection = FALSE), 
               cpi_inflator_general_date(from_date = "2014-01-01", 
                                         to_date = "2015-01-01", 
                                         useABSConnection = FALSE))
})

test_that("cpi_inflator_general_date messages", {
  expect_message(cpi_inflator_general_date(from_date = "2013",
                                           to_date = "2014"))
  expect_error(cpi_inflator_general_date(from_date = "2015-Q5",
                                         to_date = "2016-Q5"))
  expect_error(cpi_inflator_general_date(from_date = "2016-Q2",
                                         to_date = "QFG"),
               regexp = "Emergency stop: to_date",
               fixed = TRUE)
})

test_that("cpi returns reasonable forecasts", {
  skip_if_not(packageVersion("rsdmx") >= package_version("0.5.10"))
  expect_gt(cpi_inflator(from_nominal_price = 1,
                         from_fy = "2016-17",
                         to_fy = "2018-19",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE), 
            cpi_inflator(from_nominal_price = 1,
                         from_fy = "2016-17",
                         to_fy = "2017-18",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE))
  expect_gt(cpi_inflator(from_nominal_price = 1,
                         from_fy = "2016-17",
                         to_fy = "2018-19",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE), 
            cpi_inflator(from_nominal_price = 1,
                         from_fy = "2016-17",
                         to_fy = "2017-18",
                         adjustment = "none",
                         useABSConnection = TRUE,
                         allow.projection = FALSE))
  expect_lt(cpi_inflator(from_nominal_price = 1,
                         from_fy = "2012-13",
                         to_fy = "2015-16",
                         adjustment = "none",
                         useABSConnection = FALSE,
                         allow.projection = TRUE), 
            1.06)
})

test_that("ABS connection", {
  skip_on_cran()
  internal_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "none", 
                               useABSConnection = FALSE)
  external_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "none", 
                               useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
  
  internal_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "seasonal", 
                               useABSConnection = FALSE)
  external_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "seasonal", 
                               useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
  
  internal_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "trimmed", 
                               useABSConnection = FALSE)
  external_ans <- cpi_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14", 
                               adjustment = "trimmed", 
                               useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
  internal_ans <- cpi_inflator_quarters(100, 
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "trimmed", 
                                        useABSConnection = FALSE)
  external_ans <- cpi_inflator_quarters(100,
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "trimmed", 
                                        useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
  
  internal_ans <- cpi_inflator_quarters(100, 
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "seasonal", 
                                        useABSConnection = FALSE)
  external_ans <- cpi_inflator_quarters(100,
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "seasonal", 
                                        useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
  
  internal_ans <- cpi_inflator_quarters(100, 
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "none", 
                                        useABSConnection = FALSE)
  external_ans <- cpi_inflator_quarters(100,
                                        from_qtr = "1960-Q1", 
                                        to_qtr = "1961-Q1", 
                                        adjustment = "none", 
                                        useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0001)
  
})
