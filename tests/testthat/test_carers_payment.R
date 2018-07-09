context("Carers")

test_that("Carers payment", {
  expect_message(carers_payment())
  expect_equal(carers_payment(fy.year = "2017-18"), 127.1)
})
