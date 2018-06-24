context("FY")

test_that("is.fy() returns TRUE on FYs", {
  expect_true(is.fy("2012-13"))
  expect_true(is.fy("1999-00"))
  expect_true(is.fy("201415"))
})

test_that("is.fy() returns FALSE on non-FYs", {
  expect_false(is.fy("2012-14"))
  expect_false(is.fy("banana"))
  expect_false(is.fy("2012-13 was a long year"))
  expect_false(is.fy("20121313"))
  expect_false(is.fy("12-13"))
})

