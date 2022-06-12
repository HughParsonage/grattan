context("mutate ntile")

test_that("Error handling", {
  library(data.table)
  skip_if_not_installed("hutils")
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
  expect_equal(mutate_ntile(DT, "x", n = 2L, new.col = "x1", check.na = FALSE)[-5],
               # Not guaranteed:
               data.table(x = DT$x, x1 = hutils::weighted_ntile(DT$x, n = 2))[-5])
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x1", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  DT[1L, x := 3L]
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x11", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  
})



