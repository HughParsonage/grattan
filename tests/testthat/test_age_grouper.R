context("Utilities")

test_that("age_grouper delivers appropriate results", {
  expect_warning(age_grouper(5, labels = "a"))
  expect_error(age_grouper(5, breaks = 4, interval = 10))
  
  expect_equal(age_grouper(c(20, 40, 90)), 
               factor(c("Below\n25", "35-44", "75+"), levels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), ordered = TRUE))
  
  expect_equal(age_grouper(c(20, 40, 90), breaks = c(-Inf, 25, 35, 45, 55, 65, 75, Inf), labels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")), 
               factor(c("Below\n25", "35-44", "75+"), levels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), ordered = TRUE))
})

test_that("Long age_grouper", {
  y <- rep(1:100, times = 1000L)
  expect_equal(age_grouper(y),
               rep(age_grouper(1:100), times = 1000L))
  Age20M <- as.character(age_grouper(20L, below = "Below "))
  expect_equal(Age20M, "Below 25")
})

test_that("Alternate usage: years of birth", {
  expect_equal(age_grouper(c(1980, 1995, 1973), min_age = 1950, max_age = 1990),
               factor(c("1980-1989", "1990+", "1970-1979"),
                      levels = c("Below\n1950", "1950-1959", 
                                 "1960-1969", "1970-1979",
                                 "1980-1989", "1990+"),
                      ordered = TRUE))
})

test_that("Long age grouper with NAs", {
  y <- rep(c(NA, 1:100), times = 1000L)
  expect_equal(age_grouper(y),
               rep(age_grouper(c(NA, 1:100)), times = 1000L))
  Age20M <- as.character(age_grouper(20L, below = "Below "))
  expect_equal(Age20M, "Below 25")
})


