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
})