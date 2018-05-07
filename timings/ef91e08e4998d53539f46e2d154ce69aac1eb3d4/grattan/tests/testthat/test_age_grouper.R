context("Utilities")

test_that("age_grouper delivers appropriate results", {
  expect_warning(age_grouper(5, labels = "a"))
  expect_error(age_grouper(5, breaks = 4, interval = 10))
  
  expect_equal(age_grouper(c(20, 40, 90)), 
               factor(c("Below\n25", "35-44", "75+"), levels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), ordered = TRUE))
  
  expect_equal(age_grouper(c(20, 40, 90), breaks = c(-Inf, 25, 35, 45, 55, 65, 75, Inf), labels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")), 
               factor(c("Below\n25", "35-44", "75+"), levels = c("Below\n25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), ordered = TRUE))
})
