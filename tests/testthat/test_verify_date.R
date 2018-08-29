context("Verify Date")

test_that("errors", {
  expect_error(validate_date("2018-10-10", from = 2010, to = 2011),
               regexp = "`Date` contained '2018-10-10' at position 1, later than the latest permitted date: '2011-12-31'.",
               fixed = TRUE)
  expect_error(validate_date("2018-10-10", to = 2011),
               regexp = "`Date` contained '2018-10-10' at position 1, later than the latest permitted date: '2011-12-31'.",
               fixed = TRUE)
  expect_error(validate_date("2018-10-10", from = 2019),
               regexp = "`Date` contained '2018-10-10' at position 1, earlier than the earliest permitted date: '2019-01-01'.",
               fixed = TRUE)
  expect_error(validate_date("cvgvg"),
               regexp = " during coercion to Date")
  expect_error(validate_date(c("2018-10-10","cvgvg"), from = 2010, to = 2011),
               regexp = "[pP]osition 2.")
  
  expect_error(validate_date(data.frame()), 
               regexp = "was a data\\.frame")
  expect_error(validate_date(c("2018-01-01"), from = 1:2),
              "`from` must be length-one.",
              fixed = TRUE)
  expect_error(validate_date(c("2018-01-01"), to = 1:2),
               "`to` must be length-one.",
               fixed = TRUE)
  expect_error(validate_date("2018-01-01", from = 2018, to = 2017))
  
})

test_that("values", {
  x <- "2018-10-10"
  x2 <- validate_date(x)
  expect_true(x2 == x)
  expect_true(x2 == validate_date(x2))

  x <- c("2018-10-10", "2018-10-11")
  x2 <- validate_date(x)
  expect_true(all(x2 == x))
  
  x <- c("2018-10-10", "2018-10-11")
  x2 <- validate_date(x, from = 2018, to = 2019)
  expect_true(all(x2 == x))
  
  expect_error(validate_date(c("2019-01-01"), from = 2018, to = 2018))
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- validate_date(x, from = 2018)
  expect_true(all(x2 == x))
  
  x <- c("2018-10-10","2018-10-11")
  x2 <- validate_date(x, to = 2019)
  expect_true(all(x2 == x))
  
  x <- c("2013-12-31")
  x2 <- validate_date(x, from = "2012-13")
  expect_true(x == x2)
  
  x <- c("2013-12-31")
  x2 <- validate_date(x, from = "2012-12-01", to = "2013-14")
  expect_true(x == x2)
  
  x <- c("2013-12-31")
  x2 <- validate_date(x, from = as.Date("2012-12-01"), to = "2013-14")
  expect_true(x == x2)
  
})

test_that("sprintf error", {
  expect_error(validate_date("2015-06-01", 2015.25, 2016),
               regexp = "invalid format")
})
