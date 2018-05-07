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
