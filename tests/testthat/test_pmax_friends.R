context("Test pmaxC, pminC etc work as intended")

test_that("grattan functions and pmax pmin give identical results", {
  x <- rcauchy(100)
  y <- rcauchy(100)
  z <- rcauchy(100)
  a <- rcauchy(1)
  expect_equal(pmaxC(x, a), pmax(x, a))
  expect_equal(pminC(x, a), pmin(x, a))
  expect_equal(pmaxV(x, y), pmax(x, y))
  expect_equal(pminV(x, y), pmin(x, y))
  expect_equal(pmax3(x, y, z), pmax(x, pmax(y, z)))
  
  expect_equal(pmin0(c(-1, 0, 1)), 
               pmin(c(-1, 0, 1), 0))
  
  expect_error(pmax3(1, 2, 3:4))
  expect_error(pmaxV(1, 1:2))
  expect_error(pminV(1, 1:2))
})

test_that("pmaxIPint0", {
  expect_equal(pmaxIPint0(-2:2),
               hutils::if_else(-2:2 > 0, -2:2, 0L))
  expect_equal(pmaxIPint0(1:5),
               1:5)
})

test_that("pmaxCint", {
  expect_identical(pmaxCint(0:2, 1L), c(1L, 1L, 2L))
})

