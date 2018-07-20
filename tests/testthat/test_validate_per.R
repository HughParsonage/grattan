context('Validate per')

test_that("Errors", {
  expect_message(validate_per(per = 'year', missing_per = TRUE),
                 regexp = "`per` not set; calculating yearly payment.")
  expect_error(validate_per(per = NULL, missing_per = FALSE),
               regexp = "`per` was type 'NULL', but must be a string.")
  expect_error(validate_per(per = "month", missing_per = FALSE),
               regexp = "`per = 'month'` but must be one of 'year', 'fortnight', or 'quarter'.")
  expect_warning(validate_per(per = c("fortnight", "month"), missing_per = FALSE, .fortnights_per_yr = 26),
                 regexp = "`per` is provided but has length > 1 so only the first element (`per = 'fortnight'`) will be used.",
                 fixed = TRUE)
  expect_error(validate_per(per = "fortnight", missing_per = FALSE, .fortnights_per_yr = "twentysix"),
               reg_exp = "`per` was type 'character', but must be numeric.")
})

test_that("Values", {
  expect_equal(validate_per("fortnight", missing_per = FALSE, .fortnights_per_yr = 26), 
               26)
  expect_equal(suppressWarnings(validate_per("fortnight", missing_per = FALSE)), 
               26)
  expect_equal(validate_per("fortnight", missing_per = FALSE, .fortnights_per_yr = 365/14), 
               365/14)
  expect_equal(validate_per("quarter", missing_per = FALSE), 
               4)
  expect_equal(validate_per("year", missing_per = FALSE), 
               1)
})