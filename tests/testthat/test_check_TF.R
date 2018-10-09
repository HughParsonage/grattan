context("check_TF")

test_that("check_TF", {
  expect_null(check_TF(TRUE))
  expect_null(check_TF(FALSE))
  mustBe <- NA
  expect_error(check_TF(mustBe), 
               '`mustBe = NA` but must be TRUE or FALSE. Change `mustBe` to be TRUE or FALSE.')
  mustBe <- 1
  expect_error(check_TF(mustBe), 
               '`mustBe` was type double but must be logical. Change `mustBe` to be TRUE or FALSE.')
  mustBe <- c(TRUE, FALSE)
  expect_error(check_TF(mustBe), 
               '`mustBe` had length 2 but must be length-one. Change `mustBe` to be TRUE or FALSE.')
})

test_that("check_num1", {
  expect_null(check_num1(1L))
  expect_null(check_num1(1))
  
  mm <- 1:5
  expect_error(check_num1(mm),
               "`mm` had length 5 but must be length-one.",
               fixed = TRUE)
  mm <- "mm"
  expect_error(check_num1(mm),
               "`mm` was type character",
               fixed = TRUE)
  mm <- NA_real_
  expect_error(check_num1(mm),
               "`mm = NA` but must be a non-missing numeric.",
               fixed = TRUE)
})

