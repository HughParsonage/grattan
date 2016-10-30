context("utils")

test_that("unselect_", {
  y <- sample_file_1314 %>% copy %>% unselect_(.dots = "Sw_amt")
  z <- sample_file_1314 %>% copy %>% select(-Sw_amt)
  expect_identical(y, z)
})

test_that("as.numeric_unless_warning", {
  x <- c("1", "2", "3")
  y <- c("1", "2", "foo")
  expect_equal(as.numeric_unless_warning(x), 1:3)
  expect_equal(as.numeric_unless_warning(y), y)
})

test_that("coalesce", {
  expect_equal(NULL %||% 3, 3)
})

test_that("other utils", {
  expect_equal(mean_of_nonzero(c(-1, 2, 3)), 2.5)
  expect_false(is.nonnegative(c(-1, 2, 3)))
})

