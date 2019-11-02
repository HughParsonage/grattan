context("validate_fys_permitted")

expect_equal <- function(left, right, check.attributes = FALSE) {
  testthat::expect_equal(unclass(left),
                         unclass(right), 
                         check.attributes = FALSE)
}

test_that("Error handling", {
  expect_error(validate_fys_permitted(c("2015-16", "2015-17", "2010-9"), c("2015-16", "2016-17")), 
               regexp = "contained invalid FYs.",
               fixed = TRUE)
  zzz <- c("2015-16", "2015-17")
  expect_error(validate_fys_permitted(zzz, c("2015-16", "2016-17")), 
               regexp = "`zzz` contained invalid entry 2015-17 at position 2.",
               fixed = TRUE)
  zzz <- c("2015-16", "2016-17", "2017-18")
  yyy <- c("2015-16", "2016-17")
  expect_error(validate_fys_permitted(zzz, yyy), 
               regexp = "`zzz = 2017-18` was not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
  expect_error(validate_fys_permitted(c("2015-16", "2016-17", "2017-18", "2018-19"),
                                    c("2015-16", "2016-17")), 
               regexp = "were not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
})

test_that("min or max years", {
  expect_error(validate_fys_permitted("1980-81", min.yr = 1982L))
  expect_error(validate_fys_permitted("1980-81", max.yr = 1979L))
  expect_equal(validate_fys_permitted("1980-81", max.yr = 1982L), "1980-81")
  expect_equal(validate_fys_permitted("1984-85", min.yr = 1982L, max.yr = 1989L), "1984-85")
  expect_error(validate_fys_permitted(c("1980-81", "1980-80"), min.yr = 1980L),
               regexp = 'contained "1980-80" which is not a valid financial year.')
})

test_that("validation of other types", {
  expect_equal(validate_fys_permitted("1980 81"), "1980-81")
  expect_equal(validate_fys_permitted("198081"), "1980-81")
  v <- c("2015-16", "2015 16", "201516", "201516", "2003-04", "2004 05")
  a <- c("2015-16", "2015-16", "2015-16", "2015-16", "2003-04", "2004-05")
  expect_equal(validate_fys_permitted(v), a)
})


test_that("Validation memoization", {
  y <- c("2014-15", "2015-16")
  x <- validate_fys_permitted(y)
  expect_equal(x, y, check.attributes = FALSE)
  x1 <- validate_fys_permitted(x)
  expect_equal(x, x1, check.attributes = FALSE)
  x2 <- validate_fys_permitted(x, min.yr = 2000L)
  expect_equal(x2, x)
  setattr(x2, "grattan_min_yr", NULL)
  x3 <- validate_fys_permitted(x2, min.yr = 2000L)
  expect_equal(x, x3, check.attributes = FALSE)
  setattr(x2, "grattan_max_yr", NULL)
  x4 <- validate_fys_permitted(x2, min.yr = 2000L, max.yr = 2020L)
  expect_equal(x, x4, check.attributes = FALSE)
  
  expect_error(validate_fys_permitted(x2, min.yr = 2020L), 
               regexp = '`x2` contained "2014-15" which is earlier than the earliest permitted',
               fixed = TRUE)
  expect_error(validate_fys_permitted(x2, max.yr = 2010L), 
               regexp = '`x2` contained "2015-16" which is later than the latest permitte',
               fixed = TRUE)
  
})




