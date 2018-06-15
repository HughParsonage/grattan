context("Progressivity")

test_that("Error handling", {
  expect_error(progressivity(measure = "Kakwani"))
  expect_error(progressivity(measure = ""))
})

test_that("Simple examples", {
  I <- c(10e3, 20e3, 50e3, 100e3, 150e3) 
  # Regressive
  expect_lt(progressivity(income = I, tax = 0.3 * I + 100), 0)
  expect_equal(progressivity(income = I, tax = 0.3 * I), 0)
  expect_gt(progressivity(income = I, tax = 0.3 * (I - 10e3)), 0)
})
