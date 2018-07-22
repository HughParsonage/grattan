context("Carers")

test_that("Carers allowance", {
  expect_message(carers_allowance())
  expect_equal(carers_allowance(fy.year = "2017-18"), 127.1)
  expect_equal(carers_allowance(fy.year = c("2017-18", "2015-16", "2017-18", "2017-18")),
               c(127.1, 123.5, 127.1, 127.1))
  
})
