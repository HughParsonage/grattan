context("utils")

test_that("unselect_", {
  skip_if_not_installed("taxstats") 
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

test_that("prohibit_length0_vectors", {
  expect_error(prohibit_length0_vectors(NULL, 1, 1:5))
})

test_that("prohibit_vector_recyling", {
  expect_error(prohibit_vector_recycling(c(2, 2), 1, c(3, 3, 3)))
})

