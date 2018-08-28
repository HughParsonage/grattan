context(".dots.ATO")

test_that("No NAs for sample_files_all", {
  skip_if_not_installed("taxstats") 
  invisible(sample_files_all)
  expect_false(anyNA(income_tax(sample_files_all[["Taxable_Income"]], 
                                fy.year = sample_files_all[["fy.year"]], 
                                .dots.ATO = sample_files_all)))
})
