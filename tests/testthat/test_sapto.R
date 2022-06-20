test_that("sapto", {
  expect_equal(sapto(9999L, "2017-18"), 2230)
  expect_equal(sapto(c(100e3, 50e3), "2017-18", sapto.eligible = c(TRUE, FALSE)),
               c(0, 0))
})

