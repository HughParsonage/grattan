context(".dots.ATO")

test_that("No NAs for sample_files_all", {
  skip_if_not_installed("taxstats") 
  skip_on_cran()
  skip_on_appveyor()
  library(taxstats)
  sample_files_all <- get_sample_files_all()
  expect_false(anyNA(income_tax(sample_files_all[["Taxable_Income"]], 
                                fy.year = sample_files_all[["fy.year"]], 
                                .dots.ATO = sample_files_all)))
  
  expect_warning(sample_files_all[, .(income_tax(Taxable_Income, "2013-14", .dots.ATO = .SD, age = 42))],
                 regexp = "`age` is not NULL but `.dots.ATO` is supplied with an age variable, so age will be ignored.",
                 fixed = TRUE)
  
})






