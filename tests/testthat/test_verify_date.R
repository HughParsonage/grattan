context("Verify Date")

test_that("errors", {
  expect_error(verify_date("2018-10-10", from = 2010, to = 2011),
               regexp = "`Date` contained '2018-10-10' at position 1, later than the latest permitted date: '2011-01-01'.",
               fixed = TRUE)
  expect_error(verify_date("2018-10-10", to = 2011),
               regexp = "`Date` contained '2018-10-10' at position 1, later than the latest permitted date: '2011-01-01'.",
               fixed = TRUE)
  expect_error(verify_date("2018-10-10", from = 2019),
               regexp = "`Date` contained '2018-10-10' at position 1, earlier than the earliest permitted date: '2019-01-01'.",
               fixed = TRUE)
  expect_error(verify_date("cvgvg"),
               regexp = " during coercion to Date")
  expect_error(verify_date(c("2018-10-10","cvgvg"), from = 2010, to = 2011),
               regexp = "[pP]osition 2.")
})

test_that("values", {
  x <- "2018-10-10"
  x2 <- verify_date(x)
  expect_true(x2 == x)

  x <- c("2018-10-10", "2018-10-11")
  x2 <- verify_date(x)
  expect_true(all(x2 == x))
  
  x <- c("2018-10-10", "2018-10-11")
  x2 <- verify_date(x, from = 2018, to = 2019)
  expect_true(all(x2 == x))
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x, from = 2018)
  expect_true(all(x2 == x))
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- verify_date(x, to = 2019)
  expect_true(all(x2 == x))
})

test_that("sprintf error", {
  expect_error(verify_date("2015-06-01", 2015.25, 2016),
               regexp = "invalid format")
})