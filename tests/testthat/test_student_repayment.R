context("Test HECS repayments")

test_that("Error handling", {
  expect_error(student_repayment(22000 - 1, fy.year = "1988-90", debt = 100e3), regexp = "form")
  expect_error(student_repayment(22000 - 1, fy.year = "1970-71", debt = 100e3), regexp = "No data available")
})

test_that("Repayment income below threshold equal zero", {
  expect_equal(student_repayment(22000 - 1, fy.year = "1988-89", debt = 100e3), 0)
  expect_equal(student_repayment(25348 - 1, fy.year = "2003-04", debt = 100e3), 0)
  expect_equal(student_repayment(54126 - 1, fy.year = "2015-16", debt = 100e3), 0)
  expect_equal(student_repayment(54869 - 1, fy.year = "2016-17", debt = 100e3), 0)
})

test_that("Repayment never exceeds debt", {
  # dont' bother testing when repayment is zero
  debt <- repayment_income <- 0
  fy.year = "2012-13"
  while (student_repayment(repayment_income, fy.year, debt) < 1){
    repayment_income <- 30e3 * abs(rcauchy(1))
    fy.year <- sample(grattan:::hecs_tbl$fy_year, size = 1, replace = TRUE)
    debt <- 30e3 * abs(rcauchy(1))
  }
  expect_lte(student_repayment(repayment_income, fy.year, debt), debt)
})

