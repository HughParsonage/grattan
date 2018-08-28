context(".dots.ATO")

test_that("No NAs for sample_files_all", {
  skip_if_not_installed("taxstats") 
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()
  library(taxstats)
  sample_files_all <- get_sample_files_all()
  expect_false(anyNA(income_tax(sample_files_all[["Taxable_Income"]], 
                                fy.year = sample_files_all[["fy.year"]], 
                                .dots.ATO = sample_files_all)))
})

test_that("Use rolling income tax", {
  skip_if_not_installed("taxstats") 
  skip_on_cran()
  skip_on_appveyor()
  library(taxstats)
  sample_file_1314 <- copy(sample_file_1314)
  income <- sample_file_1314[["Taxable_Income"]]
  result1 <- rolling_income_tax(income, "2013-14", .dots.ATO = sample_file_1314)
  result2 <- income_tax(income, "2013-14", .dots.ATO = sample_file_1314)
  expect_equal(result1, result2)
  
  
  sample_file_1314 <- copy(sample_file_1314)
  income <- sample_file_1314[["Taxable_Income"]]
  result1 <- rolling_income_tax(income, "2014-15", .dots.ATO = sample_file_1314)
  result2 <- income_tax(income, "2014-15", .dots.ATO = sample_file_1314)
  expect_equal(result1, result2)
})

test_that("Debugger", {
  skip_if_not_installed("taxstats")
  skip_on_cran()
  skip_on_appveyor()
  library(taxstats)
  result <- 
    income_tax(sample_file_1112$Taxable_Income,
               "2013-14",
               .dots.ATO = copy(sample_file_1112),
               .debug = TRUE)
  
  expect_true(is.data.table(result))
  expect_equal(result[["income_tax"]], 
               income_tax(sample_file_1112$Taxable_Income, 
                          "2013-14", 
                          .dots.ATO = copy(sample_file_1112)))
})
