context("Verify Date")

test_that("errors", {
  expect_error(verify_date("2018-10-10", from = 2010, to = 2011),
               regexp = "`Date` had value 2018-10-10 at position 1. Ensure `Date` only includes dates between 2010 and 2011")
  expect_error(verify_date("2018-10-10", to = 2011),
               regexp = "`Date` had value 2018-10-10 at position 1. Ensure `Date` only includes dates between -Inf and 2011")
  expect_error(verify_date("2018-10-10", from = 2019),
               regexp = "`Date` had value 2018-10-10 at position 1. Ensure `Date` only includes dates between 2019 and Inf")
  expect_error(verify_date("cvgvg"),
               regexp = "`Date` had value cvgvg at position 1.")
  expect_error(verify_date(c("2018-10-10","cvgvg"), from = 2010, to = 2011),
               regexp = "`Date` had value cvgvg at position 2.")
})

test_that("values", {
  x <- "2018-10-10"
  x2 <- verify_date(x)
  expect_equal(x2, x, check.attributes = FALSE)
  expect_true(attributes(x2)$grattan_all_date)

  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x)
  expect_equal(x2, x, check.attributes = FALSE)
  expect_true(attributes(x2)$grattan_all_date)
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x, from = 2018, to = 2019)
  expect_equal(x2, x, check.attributes = FALSE)
  expect_true(attributes(x2)$grattan_all_date)
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x, from = 2018)
  expect_equal(x2, x, check.attributes = FALSE)
  expect_true(attributes(x2)$grattan_all_date)
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x, to = 2019)
  expect_equal(x2, x, check.attributes = FALSE)
  expect_true(attributes(x2)$grattan_all_date)
})