context("Superannuation variables")

library(taxstats)


test_that("Div293 tax is bounded by cap @ 25k", {
  cap1 <- 25e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5))
})
test_that("Div293 tax is bounded by cap @ 20k", {
  cap1 <- 20e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5))
})
test_that("Div293 tax is bounded by cap @ 30k", {
  cap1 <- 30e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5), info = as.character(paste0("cap1 = ", cap1)))
})

test_that("Div293 tax is bounded by an arbitrary cap", {
  caps <- sort(abs(rcauchy(2, location = 30e3, scale = 20e3)))
  cap1 <- caps[1]
  cap2 <- caps[2]
  
  age_based_cap <- sample(c(TRUE, FALSE), size = 1)
  div293_threshold <- abs(rcauchy(1, 300e3, 100e3))
  cap2_age <- sample(25:65, size = 1)
  
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, 
                                                 cap = cap1, cap2 = cap2, age_based_cap = age_based_cap, 
                                                 div293_threshold = div293_threshold, cap2_age = cap2_age)
  expect_true(all(new_sample_file$div293_tax <= new_sample_file$concessional_cap * 0.15 + .Machine$double.eps ^ 0.5), info = as.character(paste0("cap1 = ", cap1)))
})

# Adjusted Taxable Income (for surcharge purposes < 300e3)
test_that("Surchargeable income and low tax contributions less than 300,000 implies no Div293 tax", {
  caps <- sort(abs(rcauchy(2, location = 30e3, scale = 20e3)))
  cap1 <- caps[1]
  cap2 <- caps[2]
  
  age_based_cap <- sample(c(TRUE, FALSE), size = 1)
  div293_threshold <- abs(rcauchy(1, 300e3, 100e3))
  cap2_age <- sample(25:65, size = 1)
  
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, 
                                                 cap = cap1, cap2 = cap2, age_based_cap = age_based_cap, 
                                                 div293_threshold = div293_threshold, cap2_age = cap2_age)
  
  # cap1 = 82481.0762025714
  # cap2 = 12349.9120592203
  # age_based_cap = TRUE
  # div293_threshold = 177262.550400898
  # cap2_age = 65
  
  expect_true(all(new_sample_file[surchargeable_income_div293 + low_tax_contributions_div293 <= div293_threshold][["div293_tax"]] <= .Machine$double.eps ^ 0.5), 
              info = as.character(paste0("cap1 = ", cap1, "\n", 
                                         "cap2 = ", cap2, "\n",
                                         "age_based_cap = ", age_based_cap, "\n", 
                                         "div293_threshold = ", div293_threshold, "\n", 
                                         "cap2_age = ", cap2_age)))
})
