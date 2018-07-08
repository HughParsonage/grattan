context("verify_fys_permitted")

test_that("Error handling", {
  expect_error(verify_fys_permitted(c("2015-16", "2015-17", "2010-9"), c("2015-16", "2016-17")), 
               regexp = "`fy.year` contained invalid FYs.",
               fixed = TRUE)
  expect_error(verify_fys_permitted(c("2015-16", "2015-17"), c("2015-16", "2016-17")), 
               regexp = "`fy.year` contained invalid entry 2015-17 at position 2.",
               fixed = TRUE)
  expect_error(verify_fys_permitted(c("2015-16", "2016-17", "2017-18"),
                                    c("2015-16", "2016-17")), 
               regexp = "`fy.year = 2017-18` was not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
})

