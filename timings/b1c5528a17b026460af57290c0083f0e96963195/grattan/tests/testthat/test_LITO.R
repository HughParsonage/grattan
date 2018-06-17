context("LITO")

test_that("Error handling", {
  expect_error(lito(30e3, max_lito = c(445, 450)))
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

