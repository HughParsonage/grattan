context("Small business tax offset")

test_that("Example in explanatory memo", {
  expect_equal(small_business_tax_offset(100e3, 25e3, 50e3, "2015-16"), 625)
})