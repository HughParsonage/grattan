context("LITO")

test_that("Error handling", {
  expect_error(lito(30e3, max_lito = c(445, 450)))
  expect_error(lmito(30e3, fy.year = "2011-12"), 
               regexp = "fy.year.*Only these values are supported")
})

test_that("LITO", {
  expect_equal(lito(income = c(25e3, 30e3), 
                    max_lito = c(400, 500), 
                    lito_taper = c(0.015, 0.015), 
                    min_bracket = c(37000, 37000)),
               c(400, 500))
  
  expect_equal(lito(30e3, max_lito = 445, min_bracket = 37000), 
               445)
  expect_equal(lito(38000, max_lito = 1000, min_bracket = 37000, lito_taper = 0.5), 
               500)
})

test_that("LMITO", {
  # Test 2018 Budget version
  expect_equal(lmito(income  = seq(30e3, 130e3, 10e3), 
                     fy.year = "2018-19"),
               c(200, 290, 530, 530, 530, 530, 530, 380, 230, 80, 0))
  
  # Test 2019 Budget version
  
  expect_equal(lmito(income  = seq(30e3, 130e3, 10e3),
                     fy.year = "2019-20",
                     first_offset = 255,
                     thresholds = c(37e3, 48e3, 90e3, 126e3),
                     taper = c(0, 0.075, 0, -0.03)),
               c(255, 480, 1080, 1080, 1080, 1080, 1080, 780, 480, 180, 0)
               )
  
})

test_that("WATR: ALP Budget Reply 2018", {
  expect_equal(watr(seq(20e3, 120e3, by = 5e3)), 
               c(000, 350,
                 350, 350,
                 508, 770,
                 928, 928,
                 928, 928,
                 928, 928,
                 928, 928, 
                 928, 796,
                 665, 534,
                 402, 271,
                 140),
               tolerance = 2)
})

