context("GNI")

test_that("GNI returns known results", {
  expect_equal(gni_qtr("1989-12-02", roll = "nearest"),
               97145 * 1e6,
               tol = 0.5e9,
               scale = 1)
  expect_equal(gni_fy("1989-90"),
               391176000000, 
               tol = 2e9,
               scale = 1)
})

test_that("Error handling", {
  skip("All fys have full quarters in this release.")
  expect_warning(gni_fy("2018-19"))
})

context("GDP")

test_that("GDP returns known results", {
  expect_equal(gdp_qtr("1989-12-02", roll = "nearest"),
               100576000000, 
               tol = 0.1e9,
               scale = 1)
  expect_equal(gdp_fy("1989-90"),
               404889000000,
               tol = 0.5e9, 
               scale = 1)
})

test_that("Error handling", {
  library(data.table)
  skip("All fys have full quarters in this release.")
  expect_warning(gdp_fy("2018-19"))
})
