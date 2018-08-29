context("Rent assistance")

test_that("Error handling", {
  expect_message(rent_assistance(), 
                 regexp = "`fy.year` not set")
  expect_error(rent_assistance(Date = "foo"), 
               regexp = "is neither a Date object nor safely coercible as such")
  expect_error(rent_assistance(Date = "1999-01-01"), 
               regexp = "Ensure `Date` only includes dates between 2000")
  expect_error(rent_assistance(fy.year = "2016-17", n_dependants = "1.5"),
               regexp = "`n_dependants` was type character.")
  expect_error(rent_assistance(fy.year = "2016-17", n_dependants = factor(1:5)),
               regexp = "`n_dependants` was a factor.")
  expect_error(rent_assistance(fy.year = "2016-17", n_dependants = 1.5), 
               regexp = "`n_dependants` is type double and cannot be safely coerced to type integer.", 
               fixed = TRUE)
  expect_error(rent_assistance(max_rate = 10e3), 
               regexp = "`max_rate` was set, but `min_rent` is NULL.",
               fixed = TRUE)
  expect_error(rent_assistance(min_rent = 10e3), 
               regexp = "`min_rent` was set, but `max_rate` is NULL.",
               fixed = TRUE)
  expect_warning(rent_assistance(min_rent = 10e3, max_rate = 5e3, fy.year = "2015-16"),
                 regexp = "`fy.year` must not be supplied if `max_rate` and `min_rent` are.",
                 fixed = TRUE)
  expect_message(rent_assistance(min_rent = 10e3, max_rate = 5e3, n_dependants = 0L), 
               "`n_dependants`.* were supplied but are being ignored")
  expect_message(rent_assistance(min_rent = 10e3, max_rate = 5e3, has_partner = TRUE), 
               "has_partner.*supplied but are being ignored")
  expect_error(rent_assistance(min_rent = "a", max_rate = 5), regexp = "not numeric")
  expect_error(rent_assistance(min_rent = 5, max_rate = "5"), regexp = "not numeric")
  expect_warning(rent_assistance(Date = "2017-09-20", fy.year = "2017-18"),
                 regexp = "Both `Date` and `fy.year` were set. `fy.year` will be ignored.")
})

test_that("Per", {
  expect_equal(26 * rent_assistance(per = "fortnight"),
               rent_assistance(per = "year"))
  expect_equal(26 * rent_assistance(per = "fortnight", fy.year = "2015-16", n_dependants = 1L),
               rent_assistance(per = "year", fy.year = "2015-16", n_dependants = 1L))
})

test_that("Sharer's Provision", {
  expect_equal(rent_assistance(sharers_provision_applies = TRUE, is_homeowner = FALSE, has_partner = FALSE, n_dependants = 0, lives_in_sharehouse = TRUE),
               rent_assistance() * 2/3)
  expect_equal(rent_assistance(sharers_provision_applies = c(TRUE, FALSE), is_homeowner = FALSE, has_partner = FALSE, n_dependants = 0, lives_in_sharehouse = TRUE),
               rent_assistance(c(Inf,Inf)) * c(2/3, 1))
})

test_that("Rent assistance", {
  expect_equal(rent_assistance(500, max_rate = 500, min_rent = 100), 
               300)
  expect_gt(rent_assistance(500, fy.year = "2015-16", n_dependants = 1L), 
            rent_assistance(500, fy.year = "2015-16", n_dependants = 0L))
  expect_gt(rent_assistance(500, fy.year = "2015-16", n_dependants = 1), 
            rent_assistance(500, fy.year = "2015-16", n_dependants = 0))
})


test_that("Multi max rent", {
  # pminV originally caused underflow
  expect_false(is.unsorted(rent_assistance(1:500, max_rate = 100, min_rent = 50)))
})


test_that("Parents of (strictly) integer children not NA", {
  expect_false(anyNA(rent_assistance(n_dependants = 0:4, fy.year = "2017-18")))
  expect_equal(rent_assistance(100, n_dependants = 0:4, fy.year = "2017-18"), 
               rent_assistance(100, n_dependants = c(0, 1, 1, 3, 3)))
})


