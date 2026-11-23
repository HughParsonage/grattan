context("LITO")

test_that("losses and missing income agree across integer and double storage", {
  income <- c(-5L, 0L, 20000L, NA_integer_)
  for (f in list(lito, lmito)) {
    expected <-
      if (identical(f, lito)) {
        445
      } else {
        255
      }

    expect_equal(f(income, "2018-19"), c(0, expected, expected, NA_real_))
    expect_identical(f(as.double(income), "2018-19"), f(income, "2018-19"))
  }
})

test_that("LITO", {
  expect_error(lito("foo"))
  expect_equal(lito(20e3, "2015-16"), 445)
  expect_message(lito(20e3))
})


test_that("LMITO", {
  expect_message(lmito(20e3))
  # Test 2019 Budget version
  
  expect_equal(lmito(income  = seq(30e3, 130e3, 10e3),
                     fy.year = "2019-20"),
               c(255, 480, 1080, 1080, 1080, 1080, 1080, 780, 480, 180, 0))
  
})


