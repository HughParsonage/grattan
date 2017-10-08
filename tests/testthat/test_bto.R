context("BTO")

test_that("Error handling", {
  expect_error(bto(1, fy.year = "1988-89"))
})

test_that("BTO returns known results", {
  expect_equal(bto(25000, "2013-14"),
               2850) # online calculator
})

