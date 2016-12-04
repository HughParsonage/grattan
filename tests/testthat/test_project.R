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