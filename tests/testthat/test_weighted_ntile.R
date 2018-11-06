context("Utilities")

test_that("Error handling", {
  expect_error(weighted_ntile(stop("Not checked yet"), weights = -1:0), 
               regex = "contained negative values")
  expect_error(weighted_ntile(1:10, 1:5, n = 10), 
               regexp = "`length(weights) = 5`, yet `length(vector) = 10`.", 
               fixed = TRUE)
})

test_that("weighted_ntiles on integers", {
  expect_equal(weighted_ntile(1:5, weights = rep(1, 5), n = 2), c(1, 1, 1, 2, 2))
  expect_equal(weighted_ntile(1:5, weights = 1, n = 2), c(1, 1, 1, 2, 2))
  expect_equal(weighted_ntile(1:5, weights = NULL, n = 2), c(1, 1, 1, 2, 2))
  expect_equal(weighted_ntile(1:5, weights = rep(1, 5), n = 5), c(1, 2, 3, 4, 5))
  expect_equal(weighted_ntile(vector = 5:1, weights = rep(1, 5), n = 5), rev(c(1, 2, 3, 4, 5)))
  
  expect_equal(weighted_ntile(n = 2, vector = 1:4, c(1, 1, 1, 5)), c(1, 1, 1, 1))
  expect_equal(weighted_ntile(n = 4, vector = 4:1, weights = c(1, 1, 1, 5)), c(4, 4, 3, 1))
  
  expect_warning(weighted_ntile(1:5, weights = c(1, 1, 1, 2, 0), n = 5),
                 regexp = "Some weights are zero")
  expect_warning(weighted_ntile(1:5, weights = c(1, 1, 1, 2, 0), n = 5),
                 regexp = "Some ntiles greater than n")
})


test_that("weighted_ntile agrees with svyquantile", {
  skip_if_not_installed("survey")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  library(survey)
  library(dplyr)
  library(tibble)
  
  set.seed(13)
  N <- as.integer(runif(1, 1e3, 1e4))
  wts <- pmax(round(abs(rnorm(N)), 2), 0.01) # pmax(,0.01) to ensure no nonzero weights
  val <- round(abs(rnorm(N)), 2)
  n <- 10
  quantiles <- c(0:n) / n
  dummy_survey <- 
    tibble(ids = 1:N, 
           wts = wts, 
           val = val)
  

  survey_package_quantiles <-
    svydesign(data = dummy_survey, ids = ~ids, weights = ~wts) %>%
    svyquantile(design = ., x = ~val, quantiles = quantiles)
  
  survey_cut_twice <- 
    dummy_survey %>%
    mutate(survey__package_ntiles = .bincode(val,
                                             breaks = survey_package_quantiles,
                                             include.lowest = TRUE),
           grattan_package_ntiles = weighted_ntile(vector = val, weights = wts, n = n)) 
  
  
  survey_package_cut <- 
    survey_cut_twice %>%
    group_by(survey__package_ntiles) %>%
    summarise(ww = sum(wts))
  
  grattan_package_cut <- 
    survey_cut_twice %>%
    group_by(grattan_package_ntiles) %>%
    summarise(ww = sum(wts))
  
  # In a dataset cut by proper quantiles, the 
  # sum of weights within each quantile should be 
  # equal. For real-world data sets, ties etc mean
  # that they are not exactly equal. We require
  # that our function performs at least as well as 
  # package:survey's quantiles.
  expect_lte(sd(grattan_package_cut$ww), sd(survey_package_cut$ww))
})

