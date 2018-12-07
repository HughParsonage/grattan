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
