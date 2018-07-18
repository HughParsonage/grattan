context('Validate per')

test_that("Errors", {
  expect_message(validate_per(),
              regexp = "`per` not set; calculating fortnightly payment.")
  expect_error(validate_per(NULL),
               regexp = "`per` was type 'NULL', but must be a string.")
  expect_error(validate_per("month"),
               regexp = "`per = 'month'` but must be one of 'year', 'fortnight', or 'quarter'.")
  expect_warning(validate_per(c("fortnight", "month")),
               regexp = "`per` is provided but has length > 1 so only the first element (`per = 'fortnight'`) will be used.")
})
