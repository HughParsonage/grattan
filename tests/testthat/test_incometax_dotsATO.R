context(".dots.ATO")

test_that("No NAs for sample_files_all", {
  skip_if_not_installed("taxstats") 
  invisible(sample_files_all)
  expect_false(anyNA(income_tax(sample_files_all[["Taxable_Income"]], 
                                fy.year = sample_files_all[["fy.year"]], 
                                .dots.ATO = sample_files_all)))
})

test_that("Use rolling income tax", {
  skip_if_not_installed("taxstats") 
  library(taxstats)
  sample_file_1314 <- copy(sample_file_1314)
  income <- sample_file_1314[["Taxable_Income"]]
  result1 <- rolling_income_tax(income, "2013-14", .dots.ATO = sample_file_1314)
  result2 <- income_tax(income, "2013-14", .dots.ATO = sample_file_1314)
  expect_equal(result1, result2)
  
  
  sample_file_1415 <- copy(sample_file_1415_synth)
  income <- sample_file_1415[["Taxable_Income"]]
  result1 <- rolling_income_tax(income, "2014-15", .dots.ATO = copy(sample_file_1415))
  result2 <- income_tax(income, "2014-15", .dots.ATO = copy(sample_file_1415))
  expect_equal(result1, result2)
})


result <- 
  sample_file_1415 %>%
  copy %>%
  .[, tax1 := income_tax(Taxable_Income, "2014-15", .dots.ATO = copy(.))] %>%
  .[, tax2 := income_tax(Taxable_Income, rep_len("2014-15", .N), .dots.ATO = copy(.))]
