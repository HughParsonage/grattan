context("Revenue foregone")

test_that("Value of revenue_foregone", {
  library(data.table)
  dt <- data.table(new_tax = rep(1, 100), 
                   baseline_tax = integer(100), 
                   WEIGHT = 1)
  expect_equal(as.double(revenue_foregone(dt)), 100)
  expect_equal(as.double(revenue_foregone(dt, revenue_positive = FALSE)), -100)
  dt[, WEIGHT := 1e6]
  expect_output(print(revenue_foregone(dt)), '[1] "$100 million"', fixed = TRUE)
  expect_equal(print.revenue_foregone(revenue_foregone(dt, FALSE)), paste0("\u2212", "$100 million"))
  dt[, WEIGHT := 2.25e7]
  expect_output(print(revenue_foregone(dt)), '[1] "$2.2 billion"', fixed = TRUE)
  expect_equal(print.revenue_foregone(revenue_foregone(dt, FALSE)), paste0("\u2212", "$2.2 billion"))
  dt[, WEIGHT := 2.25e8]
  expect_output(print(revenue_foregone(dt)), '[1] "$22 billion"', fixed = TRUE)
  expect_equal(print.revenue_foregone(revenue_foregone(dt, FALSE)), paste0("\u2212", "$22 billion"))
})

