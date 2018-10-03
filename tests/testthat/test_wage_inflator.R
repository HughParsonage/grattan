context("Wage inflator")

test_that("Default from_fy and to_fy", {
  expect_warning(wage_inflator(), 
                 regexp = "`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  if (month(Sys.Date()) < 7) {
    expect_equal(suppressWarnings(wage_inflator()),
                 wage_inflator(from_fy = yr2fy(year(Sys.Date()) - 1),
                               to_fy = date2fy(Sys.Date())))
  } else {
    expect_equal(suppressWarnings(wage_inflator()),
                 wage_inflator(from_fy = yr2fy(year(Sys.Date())),
                               to_fy = date2fy(Sys.Date())))
  }
  
  expect_error(wage_inflator(from_fy = "2014-15"), 
               regexp = "`to_fy` is missing, with no default.")
  expect_error(wage_inflator(to_fy = "2014-15"), 
               regexp = "`from_fy` is missing, with no default.")
})

test_that("Error handling", {
  skip_on_cran()
  expect_error(lf_inflator_fy(to_fy = "2013-14"), 
               regexp = "`from_fy` is missing", 
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2013-14"), 
               regexp = "`to_fy` is missing",
               fixed = TRUE)
  expect_error(wage_inflator(1, from_fy = "2013-14", to_fy = "2045-46", allow.projection = FALSE),
               regexp = "wage index data")
  expect_error(wage_inflator(from_fy = "2017-18",
                              to_fy = "2018-19",
                              forecast.series = "custom",
                              wage.series = data.table(fy_year = c("2017-18", "2017-18"),
                                                     r = c(0, 0.123))), 
               regexp = '`wage.series$fy_year` did not have the required financial years.', 
               fixed = TRUE)
})

test_that("upper/lower higher/lower", {
  expect_gte(wage_inflator(from_fy = "2013-14", to_fy = "2030-31", forecast.series = "upper"), 
             wage_inflator(from_fy = "2013-14", to_fy = "2030-31", forecast.series = "lower"))
})

test_that("Custom wage series", {
  
  y <- wage_inflator(1, from_fy = "2017-18", to_fy = "2020-21", 
                     forecast.series = "custom", 
                     wage.series = 0.1)
  
  expect_equal(y, 1.1^3)
})
  
test_that("Custom wage series error handling", {
  # expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2017-18", 
  #                            forecast.series = "custom", 
  #                            wage.series = data.table(fy_year = c("2015-16", "2016-17", "2017-18"), 
  #                                                     r = c(42, 0.1, 0.1))),
  #              regexp = "first fy in the custom series")
  
  expect_error(wage_inflator(1, from_fy = "2015-16", to_fy = "2020-21", 
                             forecast.series = "custom", 
                             wage.series = c(1, 2)),
               regexp = "`wage.series` had length 2.",
               fixed = TRUE)
  
  expect_message(wage_inflator(1, from_fy = "2015-16", to_fy = "2018-19", 
                               forecast.series = "custom", 
                               wage.series = data.table(fy_year = c("2017-18", "2018-19"), 
                                                        r = c(2.5, 10.0))),
                 regexp = "unlikely")
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

test_that("ABS connection", {
  skip_on_cran()
  skip_on_circleci(2)
  skip_if_not(packageVersion("rsdmx") >= package_version("0.5.10"))
  
  # Minimize false on errors on travis
  skip_if(getRversion() >= "3.6")
  skip_if(getRversion() <= "3.4")
  
  internal_ans <- wage_inflator(from_fy = "2012-13", 
                                to_fy = "2013-14",
                                useABSConnection = FALSE)
  external_ans <- wage_inflator(from_fy = "2012-13", 
                               to_fy = "2013-14",
                               useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.00001, scale = 1)
})

test_that("ABS Connection (extras)", {
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()
  skip_on_circleci(2)
  internal_ans <- wage_inflator(from_fy = "2012-13", 
                                to_fy = "2020-21",
                                useABSConnection = FALSE)
  external_ans <- wage_inflator(from_fy = "2012-13", 
                                to_fy = "2020 21",
                                useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.00001, scale = 1)
  
  internal_ans <- wage_inflator(from_fy = yr2fy(2013:2016), 
                                to_fy = "2020-21",
                                useABSConnection = FALSE)
  external_ans <- wage_inflator(from_fy = yr2fy(2013:2016), 
                                to_fy = "2020 21",
                                useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.00001, scale = 1)
})

test_that("accelerated", {
  skip_on_cran()
  set.seed(1111)
  skip_on_circleci(2)
  long_fys <- to_fys <- sample(yr2fy(2005:2010), size = 2e6, replace = TRUE)
  expect_identical(wage_inflator(1, from_fy = "2004-05", to_fy = to_fys), 
                   wage_inflator(rep(1, 2e6), from_fy = "2004-05", to_fy = to_fys))
  time1 <- system.time(wage_inflator(1, from_fy = "2004-05", to_fy = to_fys))
  time2 <- system.time(wage_inflator(rep(1, 2e6), from_fy = "2004-05", to_fy = to_fys))
  expect_gt(time2[["elapsed"]] / time1[["elapsed"]], 10)
  
  a1 <- wage_inflator(1, from_fy = "2004-05", to_fy = c(to_fys, "2020-21"))
  b1 <- wage_inflator(rep(1, 2e6 + 1), from_fy = "2004-05", to_fy = c(to_fys, "2020-21"))
  expect_identical(a1, b1)
  
  a2 <- wage_inflator(1, to_fy = "2017-18", from_fy = to_fys)
  b2 <- wage_inflator(rep(1, 2e6), to_fy = "2017-18", from_fy = to_fys)
  expect_identical(a2, b2)
  
  af <- wage_inflator(1,
                      from_fy = "2004-05", to_fy = c(to_fys, "2020-21"), 
                      forecast.series = "custom",
                      wage.series = 0.02)
  bf <- wage_inflator(rep(1, 2e6 + 1),
                      from_fy = "2004-05", to_fy = c(to_fys, "2020-21"), 
                      forecast.series = "custom",
                      wage.series = 0.02)
  expect_identical(af, bf)
  
  rff <- runif(4, 0.01, 0.04)
  aff <- wage_inflator(1,
                       from_fy = "2004-05", to_fy = c(to_fys, "2020-21"), 
                       forecast.series = "custom",
                       wage.series = data.table(fy_year = yr2fy(2018:2021),
                                                r = rff))
  bff <- wage_inflator(rep(1, 2e6 + 1),
                       from_fy = "2004-05", to_fy = c(to_fys, "2020-21"), 
                       forecast.series = "custom",
                       wage.series = data.table(fy_year = yr2fy(2018:2021),
                                                r = rff))
  expect_identical(aff, bff)
})

test_that("accelerating both from and to", {
  skip_on_circleci(2)
  expect_identical(wage_inflator(from_fy = c("2005-06", "2008-09", "2006-07"),
                                 to_fy = c("2015-16", "2014-15", "2016-17")),
                   wage_inflator(from_fy = c("2005-06", "2008-09", "2006-07"),
                                 to_fy = c("2015-16", "2014-15", "2016-17"),
                                 accelerate.above = 2L))
})

test_that("verbose option", {
  skip_on_circleci(2)
  skip_if_not_installed("rlang")
  expect_output(
    rlang::with_options(
      wage_inflator(from_fy = "2014-15", to_fy = "2016-17"),
      grattan.verbose = TRUE
    ),
    "a:\\s+2014.15")
})



