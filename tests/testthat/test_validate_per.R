context('Validate per')

test_that("Errors", {
  expect_message(validate_per(per = 'year', missing_per = TRUE, .fortnights_per_yr = "26"),
                 regexp = "`per` not set; calculating yearly payment.")
  expect_error(validate_per(per = NULL, missing_per = FALSE, .fortnights_per_yr = "26"),
               regexp = "`per` was type 'NULL', but must be a string.")
  expect_error(validate_per(per = "month", missing_per = FALSE, .fortnights_per_yr = "26"),
               regexp = "`per = 'month'` but must be one of 'year', 'fortnight', or 'quarter'.")
  expect_warning(validate_per(per = c("fortnight", "month"), missing_per = FALSE, .fortnights_per_yr = "26"),
                 regexp = "`per` is provided but has length > 1 so only the first element (`per = 'fortnight'`) will be used.",
                 fixed = TRUE)
  expect_error(validate_per(per = "fortnight", missing_per = FALSE, .fortnights_per_yr = 360/26),
               reg_exp = "`.fortnights_per_yr` must be either 26 or 365/14")
  expect_error(validate_per(per = "fortnight", missing_per = FALSE, .fortnights_per_yr = "twentysix"),
               reg_exp = "`per` was type 'character', but must be numeric.")
  expect_warning(validate_per(per = "fortnight", missing_per = FALSE),
                 regexp = "`.fortnights_per_year` not set. Using 26 as default.")
})

test_that("Values", {
  expect_equal(validate_per("fortnight", missing_per = FALSE, .fortnights_per_yr = 26), 
               26)
  expect_equal(validate_per("fortnight", missing_per = FALSE, .fortnights_per_yr = 365/14), 
               365/14)
  expect_equal(validate_per("quarter", missing_per = FALSE, .fortnights_per_yr = "26"), 
               4)
  expect_equal(validate_per("year", missing_per = FALSE, .fortnights_per_yr = "26"), 
               1)
})