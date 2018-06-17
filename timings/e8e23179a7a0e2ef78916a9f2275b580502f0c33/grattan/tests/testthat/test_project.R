context("project function")

test_that("Columns do not vanish", {
  skip_if_not_installed("taxstats") 
  testH <- as.integer(sample(1:4, size = 1))
  y <- project(sample_file_1314, h = testH)
  all_zero <- function(x){
    all(abs(x) < .Machine$double.eps)
  }
  
  cols_zero <- sapply(grattan:::select_which_(y, is.numeric), all_zero)
  expect_false(any(cols_zero), info = paste0("h = ", testH, ". Col:", names(cols_zero)[cols_zero]))
})

test_that("Warnings", {
  skip_if_not_installed("taxstats")
  expect_warning(project(sample_file_1314, h = 1L, fy.year.of.sample.file = "2012-13"), regexp = "nrow")
  expect_error(project(sample_file_1314, h = 1L, fy.year.of.sample.file = "2011-12"),
               regexp = "2012.13.*2013.14")
})

test_that("Error handling", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  expect_error(project_to(sample_file_1112, "2013-14"),
               regexp = "`fy.year.of.sample.file` was not provided, yet its value could not be inferred from nrow(sample_file) = 254273. Either use a 2% sample file of the years 2012-13, 2013-14, or 2014-15 or supply `fy.year.of.sample.file` manually.",
               fixed = TRUE)
})
