context("verify_fys_permitted")

test_that("Error handling", {
  expect_error(verify_fys_permitted(c("2015-16", "2015-17", "2010-9"), c("2015-16", "2016-17")), 
               regexp = "contained invalid FYs.",
               fixed = TRUE)
  zzz <- c("2015-16", "2015-17")
  expect_error(verify_fys_permitted(zzz, c("2015-16", "2016-17")), 
               regexp = "`zzz` contained invalid entry 2015-17 at position 2.",
               fixed = TRUE)
  zzz <- c("2015-16", "2016-17", "2017-18")
  yyy <- c("2015-16", "2016-17")
  expect_error(verify_fys_permitted(zzz, yyy), 
               regexp = "`zzz = 2017-18` was not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
  expect_error(verify_fys_permitted(c("2015-16", "2016-17", "2017-18", "2018-19"),
                                    c("2015-16", "2016-17")), 
               regexp = "were not within the allowed range: 2015-16 <= fy.year <= 2016-17",
               fixed = TRUE)
})

test_that("min or max years", {
  expect_error(verify_fys_permitted("1980-81", min.yr = 1982L))
  expect_error(verify_fys_permitted("1980-81", max.yr = 1979L))
  expect_null(verify_fys_permitted("1980-81", max.yr = 1982L))
  expect_null(verify_fys_permitted("1984-85", min.yr = 1982L, max.yr = 1989L))
})



