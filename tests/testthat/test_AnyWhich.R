context("AnyWhiches")

test_that("anys", {
  expect_equal(anyGeq(1:5, 0L), 1L)
  expect_equal(anyGeq(1:5, 6L), 0L)
  expect_equal(anyGt(1:5, 0L), 1L)
  expect_equal(anyGt(1:5, 5L), 0L)
  expect_equal(anyLeq(1:5, 3L), 1L)
  expect_equal(anyLeq(1:5, 0L), 0L)
  expect_equal(anyLt(1:5, 2L), 1L)
  expect_equal(anyLt(1:5, 1L), 0L)
  expect_equal(anyEqual(1:5, 1L), 1L)
  expect_equal(anyEqual(1:5, 6L), 0L)
  expect_equal(anyNotEqual(1:5, 1L), 2L)
  expect_equal(anyNotEqual(1:5, 0L), 1L)
  expect_equal(anyNotEqual(rep(1L, 5L), 1L), 0L)
  
  expect_error(AnyWhich(1L, 1L, TRUE, TRUE, TRUE), 
               regexp = "gt and lt were both TRUE")
})


test_that("anyOutside", {
  expect_equal(anyOutside(1:12, 1L, 12L), 0L)
  expect_equal(anyOutside(1:12, 1L, 11L), 12L)
  expect_equal(anyOutside(1:12, 2L, 11L), 1L)
  expect_equal(anyOutside(2:12, 1L, 11L), 11L)
})

