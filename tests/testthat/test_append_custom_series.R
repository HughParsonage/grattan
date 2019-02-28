context("append series")

test_that("Errors don't refer to internal helper functions", {
  expect_error(lf_inflator_fy(from_fy = "2016-17",
                              to_fy = "2020-21",
                              forecast.series = "custom"),
               regexp = '`lf.series = NULL`, yet `forecast.series = "custom"`.',
               fixed = TRUE)
  Call <- 
    tryCatch(lf_inflator_fy(from_fy = "2016-17",
                            to_fy = "2020-21",
                            forecast.series = "custom"),
             error = function(e) {
               paste0(as.character(deparse(e$c)),
                      collapse = " ")
             })
  expect_false(grepl("append_custom_series", Call))
  expect_false(grepl("standardize_custom_series", Call))
})


test_that("As applied with inflators", {
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom"),
               regexp = '`wage.series = NULL`, yet `forecast.series = "custom"`.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = sqrt),
               regexp = '`wage.series` had class function',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = "sqrt"),
               regexp = '`wage.series` was type character. If using `wage.series` as an atomic vector, ensure it is a single numeric vector.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(1, 2)),
               regexp = '`wage.series` is a list with no names.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(a = 1)),
               regexp = 'had fewer than 2 names.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(a = 1, b = 1)),
               regexp = 'had first name "a"',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(fy_year = 1, b = 1)),
               regexp = 'had second name "b"',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(fy_year = "2014-15", r = 1:2)),
               regexp = 'was a list with mismatching lengths:',
               fixed = TRUE)
  
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(fy_year = yr2fy(2021:2018), r = 1:4/100)),
               regexp = '`wage.series$fy_year` had the required financial years but not in the correct order.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2020-21",
                             forecast.series = "custom",
                             wage.series = list(fy_year = c("2016-17", "2017-18", "2019-20", "2018-19", "2020-21"),
                                                r = 1:5/100)),
               regexp = '`wage.series$fy_year` had the required financial years but not in the correct order.',
               fixed = TRUE)
  expect_error(wage_inflator(from_fy = "2015-16", to_fy = "2019-20",
                             forecast.series = "custom",
                             wage.series = list(fy_year = c("2019-20", "2018-19"),
                                                r = 1:2/100)),
               regexp = '`wage.series$fy_year` had the required financial years but not in the correct order.',
               fixed = TRUE)
  
  expect_error(lf_inflator_fy(from_fy = "2015-16", to_fy = "2020-21",
                              forecast.series = "custom",
                              lf.series = list(fy_year = "2020-21", r = 0.01)),
               regexp = "`lf.series$fy_year` did not have the required financial years.",
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2015-16", to_fy = "2020-21",
                              forecast.series = "custom",
                              lf.series = list(fy_year = c("2018-19", "2020-21", "2019-20"),
                                               r = 1:3/100)),
               regexp = "`lf.series$fy_year` had the required financial years but not in the correct order.",
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2015-16", to_fy = next_fy(h = 3L),
                              forecast.series = "custom",
                              lf.series = list(fy_year = c(next_fy(h = 3L), next_fy(h = 2L)),
                                               r = 1:2/100)),
               regexp = "`lf.series$fy_year` did not have the required financial years.",
               fixed = TRUE)
  expect_error(lf_inflator_fy(from_fy = "2015-16", to_fy = next_fy(h = 3L),
                              forecast.series = "custom",
                              lf.series = data.table(fy_year = c(next_fy(h = 3L), next_fy(h = 2L)),
                                                     r = 1:2/100)),
               regexp = "`lf.series$fy_year` did not have the required financial years.",
               fixed = TRUE)
})



