context("AWOTE")

test_that("Error handling", {
  expect_error(awote(isMale = "yes"))
  expect_error(awote(isAdult = "yes"))
  expect_error(awote(isOrdinary = "yes"))
  expect_error(awote(isAdult = NA))
  expect_error(awote(isOrdinary = NA))
})

test_that("Message", {
  expect_message(awote(), 
                 regexp = "both NULL so using")
})

test_that("AWOTE fy", {
  library(data.table)
  expect_true(between(awote(Date = fy2date("2013-14")),
                      awote(fy.year = "2012-13"),
                      awote(fy.year = "2014-15")))
  expect_equal(awote_fy("2014-15", isMale = NA, isAdult = TRUE, isOrdinary = TRUE),
               awote(fy.year = "2014-15"))
})
