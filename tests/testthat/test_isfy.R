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

test_that("Other fy utils", {
  expect_equal(fy.year(2012), "2011-12")
  # Not an FY
  expect_error(fy2yr("2014-16"))
  expect_error(fy2date("2014-16"))
  
  expect_equal(fy2date("2012-13"), as.Date("2013-06-30"))
  
  expect_equal(fyback("2012-13", back=10),"2022-2023")
  expect_equal(fyforward("2012-13", forward=10),"2022-23")
})

test_that("all_fy", {
  expect_true(all_fy(c("2000-01", "2010-11", "2013-14", "2020-21")))
})

test_that("is_fy2", {
  expect_true(all(is_fy2(c("2000-01", "2010-11", "2013-14", "2020-21"))))
})


