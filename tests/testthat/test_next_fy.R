context("Next fy")


test_that("next_fy plus and minus", {
  expect_equal(next_fy("2015-16"), "2016-17")
  expect_equal(next_fy(c("2015-16", "2018-19")),
               c("2016-17", "2019-20"))
  expect_equal(next_fy("1999-00", h = 3), "2002-03")
  expect_equal(next_fy("2002-03", h = -3), "1999-00")
})

test_that("prev_fy", {
  expect_equal(prev_fy("2015-16"), "2014-15")
  expect_equal(prev_fy(c("2016-17", "2019-20")),
               c("2015-16", "2018-19"))
})

