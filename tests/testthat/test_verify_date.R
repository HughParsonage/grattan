context("Verify Date")

test_that("errors", {
  expect_silent(verify_date("2018-10-10"))#expect no output
  expect_error(verify_date("2018-10-10", from = 2010, to = 2011),
               regexp = "`Date` had value 2018-10-10 at position 1. Ensure `Date` only includes dates between 2010 and 2011")
  expect_error(verify_date("cvgvg"),
               regexp = "`Date` had value cvgvg at position 1.")
  expect_error(verify_date(c("2018-10-10","cvgvg"), from = 2010, to = 2011),
               regexp = "`Date` had value cvgvg at position 2.")
})
