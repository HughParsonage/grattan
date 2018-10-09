context("CPI return correct results")

test_that("Default from_fy and to_fy", {
  skip_on_circleci(1)
  
  expect_warning(cpi_inflator(), 
                 regexp = "`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  if (data.table::month(Sys.Date()) < 7) {
    expect_equal(suppressWarnings(cpi_inflator()),
                 cpi_inflator(from_fy = yr2fy(year(Sys.Date()) - 1),
                              to_fy = date2fy(Sys.Date())))
  } else {
    expect_equal(suppressWarnings(cpi_inflator()),
                 cpi_inflator(from_fy = yr2fy(year(Sys.Date())),
                              to_fy = date2fy(Sys.Date())))
  }
})

test_that("Errors", {
  skip_on_circleci(1)
  
  expect_error(cpi_inflator(to_fy = "2013-14"), 
               regexp = "`from_fy` is missing", 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14"), 
               regexp = "`to_fy` is missing",
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14", to_fy = "2040-41",
                            allow.projection = FALSE), 
               regexp = '`allow.projection = FALSE`, yet `to_fy = "2040-41"`', 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = "2013-14", to_fy = c("2015-16", "2040-41"),
                            allow.projection = FALSE), 
               regexp = '`allow.projection = FALSE`, yet `to_fy` contained "2040-41"', 
               fixed = TRUE)
  expect_error(cpi_inflator_quarters(from_qtr = "2015-Q1", to_qtr = "2015-01-01"), 
               regexp = "Dates must be in quarters.", 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = c("1920-21"),
                            to_fy = "2012-13",
                            adjustment = "none"),
               regexp = '`from_fy = "1920-21"` which is earlier than the first instance of the unadjusted CPI: "1948-49".', 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = c("2011-12", rep_len("1920-21", 3)),
                            to_fy = "2012-13",
                            adjustment = "none"),
               regexp = '`from_fy` contained "1920-21" which is earlier than the first instance of the unadjusted CPI: "1948-49".', 
               fixed = TRUE)
  
  expect_error(cpi_inflator(from_fy = c("1920-21"),
                            to_fy = "2012-13",
                            adjustment = "seasonal"),
               regexp = '`from_fy = "1920-21"` which is earlier than the first instance of the seasonally adjusted CPI: "1986-87"', 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = c("2011-12", rep_len("1920-21", 3)),
                            to_fy = "2012-13",
                            adjustment = "seasonal"),
               regexp = '`from_fy` contained "1920-21" which is earlier than the first instance of the seasonally adjusted CPI: "1986-87"', 
               fixed = TRUE)
  
  expect_error(cpi_inflator(from_fy = c("1920-21"),
                            to_fy = "2012-13",
                            adjustment = "trimmed.mean"),
               regexp = '`from_fy = "1920-21"` which is earlier than the first instance of the trimmed mean CPI: "2002-03"', 
               fixed = TRUE)
  expect_error(cpi_inflator(from_fy = c("2011-12", rep_len("1920-21", 3)),
                            to_fy = "2012-13",
                            adjustment = "trimmed.mean"),
               regexp = '`from_fy` contained "1920-21" which is earlier than the first instance of the trimmed mean CPI: "2002-03"', 
               fixed = TRUE)
  
  expect_error(cpi_inflator(from_fy = "2010-12", to_fy = "2015-16"),
               regexp = '`from_fy = "2010-12"` was not a valid financial year.',
               fixed = TRUE)
  expect_error(cpi_inflator(to_fy = "2010-12", from_fy = "2015-16"),
               regexp = '`to_fy = "2010-12"` was not a valid financial year.',
               fixed = TRUE)
  expect_warning(cpi_inflator(double(0), character(0), character(0)), 
                 "Zero")
  
  expect_error(cpi_inflator(from_fy = raw(1), to_fy = raw(1)), 
               regexp = "from_fy.*type.*raw")
  expect_error(cpi_inflator(from_fy = "2015-16", to_fy = raw(1)), 
               regexp = "`to_fy` was type raw, but must be type character.")
  
  expect_error(cpi_inflator(from_fy = c("2015-16", "aa", "bb"), to_fy = "2015-16"), 
               regexp = '`from_fy` contained "aa" which is not a valid financial year.', 
               fixed = TRUE)
  
  expect_error(cpi_inflator(from_fy = c("2015-16", "2015-16", "2015-16"),
                            to_fy = c("2015-16", "x", "y")), 
               regexp = '`to_fy` contained "x" which is not a valid financial year.', 
               fixed = TRUE)
  
})

test_that("FY corner cases", {
  skip_on_circleci(1)
  
  expect_equal(cpi_inflator(from_fy = "2014-15", to_fy = "201314"),
               cpi_inflator(from_fy = "2014-15", to_fy = "2013-14"))
})

test_that("cpi returns known results", {
  skip_on_circleci(1)
  
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
  skip_on_circleci(1)
  
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
  skip_on_circleci(1)
  
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
  skip_on_circleci(1)
  
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
  skip_on_circleci(1)
  
  skip_if_not(packageVersion("rsdmx") >= package_version("0.5.10"))
  travis_release_not_pr <- 
    identical(Sys.getenv("TRAVIS"), "true") &&
    !identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release") &&
    identical(Sys.getenv("TRAVIS_PULL_REQUEST"), "true")
  skip_if(travis_release_not_pr)
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
  skip_on_circleci(1)
  
  skip_on_cran()
  travis_release_not_pr <- 
    identical(Sys.getenv("TRAVIS"), "true") &&
    !identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release") &&
    identical(Sys.getenv("TRAVIS_PULL_REQUEST"), "true")
  skip_if(travis_release_not_pr)
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

test_that("cpi accelerated", {
  skip_on_circleci(1)
  
  skip_on_cran()
  set.seed(5)
  long_tos <- long_froms <- sample(yr2fy(2005:2010), size = 2e6, replace = TRUE)
  expect_identical(cpi_inflator(from_nominal_price = rep(2, 2e6), 
                                from_fy = long_froms, 
                                to_fy = "2015-16"), 
                   cpi_inflator(from_nominal_price = 2, 
                                from_fy = long_froms, 
                                to_fy = "2015-16"))
  expect_identical(cpi_inflator(from_nominal_price = rep(2, 2e6), 
                                from_fy = "1999-00", 
                                to_fy = long_tos), 
                   cpi_inflator(from_nominal_price = 2, 
                                from_fy = "1999-00", 
                                to_fy = long_tos))
  time1 <- system.time(cpi_inflator(1, from_fy = "2004-05", to_fy = long_tos))
  time2 <- system.time(cpi_inflator(rep(1, 2e6), from_fy = "2004-05", to_fy = long_tos))
  expect_gt(time2[["elapsed"]] / time1[["elapsed"]], 10)
  
})


test_that("cpi accelerated but both to and from multilength", {
  skip_on_circleci(1)
  
  expect_identical(cpi_inflator(from_fy = c("2005-06", "2008-09", "2006-07"),
                                to_fy = c("2015-16", "2014-15", "2016-17")),
                   cpi_inflator(from_fy = c("2005-06", "2008-09", "2006-07"),
                                to_fy = c("2015-16", "2014-15", "2016-17"),
                                accelerate.above = 2L))
})


test_that("cpi different lengths", {
  skip_on_circleci(1)
  
  expect_identical(cpi_inflator(1,
                                to_fy = c("2004-05", "2007-08", "2009-10", "2009-10", "2007-08"),
                                from_fy = "1999-00"),
                   cpi_inflator(1,
                                to_fy = c("2004-05", "2007-08", "2009-10", "2009-10", "2007-08"),
                                from_fy = rep("1999-00", 5)))
  expect_identical(cpi_inflator(1,
                                to_fy = c("2004-05", "2007-08", "2009-10", "2009-10", "2007-08"),
                                from_fy = "1999-00"),
                   cpi_inflator(rep(1, 5),
                                to_fy = c("2004-05", "2007-08", "2009-10", "2009-10", "2007-08"),
                                from_fy = rep("1999-00", 5)))
})



