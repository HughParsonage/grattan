context("Financial functions")

test_that("Error handling", {
  expect_error(irr(c(1, 1)))
})

test_that("Matches Excel", {
  expect_equal(npv(0.05, values = c(4, 3, 2, 1)), 9.08098991675279)
  expect_equal(irr(c(-1, 1)), 0)
  expect_equal(round(pv(0.05, 3, 1), 2), -2.72)
  expect_equal(round(pmt(0.05, 3, 5), 2), -1.84)
  expect_equal(round(fv(0.05, 5, 2), 2), -11.05)
})
