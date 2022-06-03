context("Offset")

test_that("Offset basics", {
  
  # Triangle
  expect_equal(Offset(0.5, 1, 0, 1), 0.5)
  expect_equal(Offset(1.0, 1, 0, 1), 0.0)
  expect_equal(Offset(0.0, 1, 0, 1), 1.0)
  expect_equal(Offset(c(0.0, 0.5, 1.0), 1, 0, 1), c(1.0, 0.5, 0.0))
  
  # Nonzero a
  expect_equal(Offset(c(0, 250, 500, 800, 2499, 2500, 2501),
                      y = 200,
                      a = 500,
                      m = 0.1),
               c(200, 200, 200, 170, 0.1, 0, 0))
  
  the.seed <- sample.int(100000L, size = 1L)
  set.seed(the.seed)
  A <- abs(rcauchy(1))
  X <- runif(10, 0, A)
  Y <- abs(rcauchy(1))
  M <- abs(rcauchy(1))
  expect_equal(Offset(x = X, y = Y, a = A, m = M),
               rep_len(Y, 10), info = the.seed)
  
})

test_that("Offset internals", {
  expect_equal(nOffset_upper_threshold( set_offsets( set_offset(offset_1st = 255L, thresholds =as.integer(c(37e3, 48e3, 90e3)), tapers = c(-0.075, 0, 0.03)))), 
               126e3)
  expect_equal(nOffset_upper_threshold( set_offsets( set_offset(offset_1st = 675L, thresholds =as.integer(c(37e3, 48e3, 90e3, 126e3)), tapers = c(-0.075, 0, 0.03, Inf)))), 
               126e3)
})
