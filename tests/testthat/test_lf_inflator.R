context("lf inflator")

test_that("Default from_fy and to_fy", {
  expect_warning(lf_inflator_fy(), 
                 regexp = "`from_fy` and `to_fy` are missing, using previous and current financial years respectively")
  if (data.table::month(Sys.Date()) < 7) {
    expect_equal(suppressWarnings(lf_inflator_fy()),
                 lf_inflator_fy(from_fy = yr2fy(year(Sys.Date()) - 1),
                                to_fy = date2fy(Sys.Date())))
  } else {
    expect_equal(suppressWarnings(lf_inflator_fy()),
                 lf_inflator_fy(from_fy = yr2fy(year(Sys.Date())),
                                to_fy = date2fy(Sys.Date())))
  } 
})


test_that("Error handling", {
  skip_on_cran()
  skip_if_not(identical(date2fy(Sys.Date()), "2020-21"))
  expect_error(lf_inflator_fy(to_fy = "2013-14"), 
               regexp = "`from_fy` is missing", 
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2013-14"), 
               regexp = "`to_fy` is missing",
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2012-13", to_fy = "2099-00", allow.projection = FALSE),
               regexp = "to_fy are in labour force data")
  expect_error(lf_inflator_fy(from_fy = "2012-13", to_fy = "2025-26", 
                              allow.projection = TRUE, 
                              forecast.series = "custom", 
                              lf.series = c(1:2)), 
               regexp = "`lf.series` had length 2. If using `lf.series` as an atomic vector, ensure it is a single numeric vector.", 
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2012-13", to_fy = "2025-26", 
                              allow.projection = TRUE, 
                              forecast.series = "custom", 
                              lf.series = data.table(fy_year = "2013-14", 
                                                     r = 0.2)), 
               regexp = "`lf.series$fy_year` did not have the required financial years.", 
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2018-19",
                              to_fy = next_fy(h = 2),
                              forecast.series = "custom",
                              lf.series = data.table(fy_year = c("2019-20", "2020-21"),
                                                     r = c(0, 0.123))), 
               regexp = '`lf.series$fy_year` did not have the required financial years.', 
               fixed = TRUE)
  
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
  skip_on_cran()
  skip("lf inflator using trend")
  expect_equal(lf_inflator(from_date = "1981-01-01", to_date = "1981-02-01"), 1.00124729250057, tol = 0.001)
})

test_that("lf_inflator returns long", {
  skip_on_cran()
  skip("lf inflator using trend")
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

test_that("Custom lf series", {
  skip_if_not(identical(date2fy(Sys.Date()), "2019-20"))
  expect_message(lf_inflator_fy(from_fy = "2022-23", 
                                to_fy = "2024-25",
                                forecast.series = "custom",
                                lf.series = 2),
                 regexp = "unlikely")
  expect_message(lf_inflator_fy(from_fy = "2018-19", 
                                to_fy = "2024-25",
                                forecast.series = "custom",
                                lf.series = 2),
                 regexp = "unlikely")
  
  y <- lf_inflator_fy(1, from_fy = "2022-23", to_fy = "2024-25", 
                      forecast.series = "custom", lf.series = 0.10)
  
  expect_equal(y, 1.1^2)
  
  y_custom_series <-
    lf_inflator_fy(from_fy = next_fy(),
                   to_fy = next_fy(h = 2),
                   forecast.series = "custom",
                   lf.series = data.table(fy_year = next_fy(h = 1:2),
                                          r = c(0, 0.123)))
  
  expect_equal(y_custom_series, 1.123)
})

test_that("ABS connection", {
  skip("ABS not available")
  skip_if_not_installed("rsdmx")
  skip_if_not(packageVersion("rsdmx") >= package_version("0.5.10"))
  skip_on_cran()
  skip_if_not(Sys.Date() > "2018-01-01")
  internal_ans <- lf_inflator_fy(from_fy = "2012-13", to_fy = "2013-14")
  external_ans <- lf_inflator_fy(from_fy = "2012-13", to_fy = "2013-14",
                                 useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0005)
  
  internal_ans <- lf_inflator(from_date = "2009-06-30", to_date = "2014-06-30")
  
  external_ans <- lf_inflator(from_date = "2009-06-30", to_date = "2014-06-30",
                              useABSConnection = TRUE)
  
  expect_equal(internal_ans, external_ans, tol = 0.0005)
})

test_that("accelerate", {
  skip_on_cran()
  
  set.seed(6)
  long_tos <- long_froms <- sample(yr2fy(2005:2010), size = 2e6, replace = TRUE)
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = long_froms, 
                                  to_fy = "2015-16"), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = long_froms, 
                                  to_fy = "2015-16"))
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = "1999-00", 
                                  to_fy = long_tos), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = "1999-00", 
                                  to_fy = long_tos))
  if (NOR(identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release"),
          identical(Sys.getenv("APPVEYOR"), "True") && data.table::second(Sys.time()) > 30)) {
    for (i in 1:3) {
      time1 <- system.time(lf_inflator_fy(1, from_fy = "2004-05", to_fy = long_tos))
      time2 <- system.time(lf_inflator_fy(rep(1, 2e6), from_fy = "2004-05", to_fy = long_tos))
      time1_time2 <- time2[["elapsed"]] / time1[["elapsed"]]
      if (time1_time2 > 10) {
        break
      }
    }
    expect_gt(time2[["elapsed"]] / time1[["elapsed"]], 10)
  }
  
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = long_froms, 
                                  to_fy = "2020-21"), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = long_froms, 
                                  to_fy = "2020-21"))
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = "1999-00", 
                                  to_fy = long_tos), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = "1999-00", 
                                  to_fy = long_tos))
  
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = long_froms, 
                                  to_fy = "2020-21",
                                  use.month = 2L), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = long_froms, 
                                  to_fy = "2020-21",
                                  use.month = 2L))
  
  expect_identical(lf_inflator_fy(labour_force = rep(2, 2e6), 
                                  from_fy = long_froms, 
                                  to_fy = "2018-19",
                                  forecast.series = "custom", 
                                  lf.series = data.table(fy_year = c("2018-19", "2019-20"),
                                                         r = c(0, 0.01))), 
                   lf_inflator_fy(labour_force = 2, 
                                  from_fy = long_froms, 
                                  to_fy = "2018-19",
                                  forecast.series = "custom", 
                                  lf.series = data.table(fy_year = c("2018-19", "2019-20"),
                                                         r = c(0, 0.01))))
  
})

test_that("accelerating both from and to", {
  expect_identical(lf_inflator_fy(from_fy = c("2005-06", "2008-09", "2006-07"),
                                  to_fy = c("2015-16", "2014-15", "2016-17")),
                   lf_inflator_fy(from_fy = c("2005-06", "2008-09", "2006-07"),
                                  to_fy = c("2015-16", "2014-15", "2016-17"),
                                  accelerate.above = 2L))
})

test_that("lf_indices", {
  skip_on_cran()
  library(data.table)
  LL_inds <- as.data.table(grattan:::lf_trend)
  LL_inds[, obsDate := as.Date(paste0(obsTime, "-01"))]
  LL_inds_month1 <- LL_inds[month(obsDate) == 1L]
  LL_inds_month1[, fy_year := date2fy(obsDate)]
  expect_equal(lf_inflator_fy(from_fy = "2014-15", to_fy = "2015-16"),
               lf_inflator_fy(from_fy = "2014-15", to_fy = "2015-16",
                              .lf_indices = LL_inds_month1))
  
  
})

test_that("deflator", {
  skip_if_not_installed("rlang")
  
  rlang::with_options({
    expect_output(lf_inflator_fy(from_fy = "2014-15", to_fy = "2013-14"),
                  "a:\\s+2014.15")
    expect_output(lf_inflator_fy(to_fy = "2014-15", from_fy = "2013-14"),
                  "a:\\s+2013.14")
  },
  grattan.verbose = TRUE)
  expect_equal(lf_inflator_fy(from_fy = "2014-15", to_fy = "2013-14"),
               1 / lf_inflator_fy(to_fy = "2014-15", from_fy = "2013-14"))
})


