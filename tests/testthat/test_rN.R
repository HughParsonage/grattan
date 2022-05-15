test_that("do_rN", {
  expect_equal(do_rN(c(NA, NA), integer(2)), integer(2))
  expect_equal(do_rN(c(NA, NA_integer_), integer(2)), integer(2))
  expect_equal(do_rN(c(NA, NA_integer_, 0L), integer(3)), integer(3))
  
  expect_equal(do_rN(1:5, integer(5)), 1:5)
  expect_equal(do_rN(c(NaN, -1e10, 0, 1e10), integer(4)), c(0L, -as.integer(2^31 - 1), 0L, as.integer(2^31 - 1)))
})
