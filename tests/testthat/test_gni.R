context("GNI")

test_that("GNI returns known results", {
  expect_equal(gni_qtr("1989-12-02", roll = "nearest"), 97145 * 1e6)
  expect_equal(gni_fy("1989-90"), 99929 * 4 * 1e6)
})