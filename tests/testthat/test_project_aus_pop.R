context("project aus pop")

test_that("project using australian population", {
  skip_if_not_installed("taxstats")
  skip_if_not_installed("data.table")
  skip_if_not_installed("magrittr")
  library(magrittr)
  library(data.table)
  library(taxstats)
  
 
  # sample_file_1516[, .(WEIGHT = .N * 100), keyby = "age_range"]
  Actual_201516 <- 
    data.table(age_range = 0:11, 
               WEIGHT = c(802750, 606600, 918950, 1150250, 1266400, 1313150, 
                          1348750, 1343100, 1509050, 1498700, 1266700, 457550))
  
  s1516_orig <- project(sample_file_1314, h = 2L)
  s1516_new <- project(sample_file_1314, h = 2L, use_age_pop_forecast = TRUE)
  
  
  err_orig <- 
    s1516_orig[, .(WEIGHT = sum(WEIGHT)), keyby = "age_range"] %>%
    .[Actual_201516, on = "age_range"] %>%
    .[, mean(abs(WEIGHT - i.WEIGHT))]
    
    # Should be a bit better
  err_new <- 
    s1516_new[, .(WEIGHT = sum(WEIGHT)), keyby = "age_range"] %>%
    .[Actual_201516, on = "age_range"] %>%
    .[, mean(abs(WEIGHT - i.WEIGHT))]

  expect_lte(err_new, err_orig)
})
