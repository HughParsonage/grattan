context("mutate ntile")

test_that("Error handling", {
  library(data.table)
  DT <- data.table(x = 1:101)
  expect_error(mutate_ntile(DT, n = 1:2),
               regexp = "length(n) = 2", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, n = "1"),
               regexp = "type character", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, n = 2.5),
               regexp = "n != as.integer(n)", 
               fixed = TRUE)
  
  
  expect_error(mutate_ntile(DT, n = 2),
               regexp = "`col` is missing", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, x, n = 17),
               regexp = "no default column suffix is supported", 
               fixed = TRUE)
  
  expect_error(mutate_ntile(DT, x, n = 10, new.col = "x", overwrite = FALSE),
               regexp = "overwrite = TRUE", 
               fixed = TRUE)
  
  expect_message(mutate_ntile(DT, x, n = 1, new.col = "Single1"),
                 regexp = "adding a column of 1s",
                 fixed = TRUE)
  
  expect_error(mutate_ntile(DT, x, n = -1, new.col = "xy", overwrite = FALSE),
               regexp = "must be a single positive whole number", 
               fixed = TRUE)
  
  expect_error(mutate_ntile(as.list(DT), x, n = 2, new.col = "xy", overwrite = FALSE),
               regexp = "must be a data.frame", 
               fixed = TRUE)
  DT <- data.table(x = 1:10)
  DT[5L, x := NA_integer_]
  expect_equal(mutate_ntile(DT, "x", n = 2L, new.col = "x1", check.na = FALSE),
               # Not guaranteed:
               data.table(x = DT$x, x1 = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2)))
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x1", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
})

test_that(".ntile", {
  expect_error(.ntile(c(1, 2, 5, 4), n = 1, check.sorted = TRUE), 
               regexp = "`x` must be already sorted",
               fixed = TRUE)
  expect_error(.ntile(c(1, 2, 5, 4, NA), n = 1, check.na = TRUE), 
               regexp = "anyNA(x)` is TRUE",
               fixed = TRUE)
})

test_that("Corresponds to dplyr::ntile", {
  skip_if_not_installed("dplyr")
  library(data.table)
  DT <- data.table(x = 1:101)
  expect_identical(mutate_ntile(DT, x, n = 5)[["xQuintile"]], 
                   dplyr::ntile(1:101, n = 5))
  setkey(DT, x)
  expect_identical(mutate_ntile(DT, x, n = 10)[["xDecile"]], 
                   dplyr::ntile(1:101, n = 10))
  # make uneven
  DT[5L, x := 0L]
  expect_identical(mutate_ntile(DT, x, n = 4)[["xQuartile"]], 
                   dplyr::ntile(1:101, n = 4))
  
  
  
})

test_that("data frames", {
  DT <- data.frame(x = 1:59)
  expect_identical(mutate_ntile(DT, x, n = 10)[["xDecile"]], 
                   dplyr::ntile(1:59, n = 10))
  expect_identical(mutate_ntile(DT, x, n = 1, new.col = "y")[["y"]], 
                   dplyr::ntile(1:59, n = 1))
  DT <- data.frame(xy = rev(1:59))
  
  
  
})

