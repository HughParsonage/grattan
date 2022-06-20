test_that("sapto", {
  expect_equal(sapto(9999L, "2017-18"), 2230)
  expect_equal(sapto(c(100e3, 50e3), "2017-18", sapto.eligible = c(TRUE, FALSE)),
               c(0, 0))
  expect_error(sapto(100e3, "2017-18", fill = "a"), "fill")
})

test_that("valid sapto", {
  expect_error(System(2018L, sapto_pension_age = 666, fix = 0L), "666")
  expect_warning(System(2018L, sapto_pension_age = 666, fix = 1L), "666")
})

