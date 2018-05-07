context("BTO")

test_that("Error handling", {
  expect_error(bto(1, fy.year = "1988-89"))
})

test_that("BTO returns known results", {
  expect_equal(bto(25000, "2013-14"),
               2850) # online calculator
  
  expect_equal(bto(10e3, rate1 = 0.1, benefit_threshold = 9e3), 
               100)
  
  bto_199697 <- bto(6008, "2005-06")
  expect_equal(bto_199697, 2)
})

