context("Utilities")

test_that("weighted_ntiles", {
  expect_equal(weighted_ntile(1:5, weights = rep(1, 5), n = 2), c(1, 1, 1, 2, 2))
  expect_equal(weighted_ntile(1:5, weights = rep(1, 5), n = 5), c(1, 2, 3, 4, 5))
  expect_equal(weighted_ntile(vector = 5:1, weights = rep(1, 5), n = 5), rev(c(1, 2, 3, 4, 5)))
  
  expect_equal(weighted_ntile(n = 2, vector = 1:4, c(1, 1, 1, 5)), c(1, 1, 1, 1))
  expect_equal(weighted_ntile(n = 4, vector = 4:1, weights = c(1, 1, 1, 5)), c(4, 4, 3, 1))
  
  expect_warning(weighted_ntile(1:5, weights = c(1, 1, 1, 2, 0), n = 5))
})
