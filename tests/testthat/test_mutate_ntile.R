context("mutate ntile")

test_that("Error handling", {
  skip_if_not_installed("dplyr")
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
  expect_equal(mutate_ntile(DT, "x", n = 2L, new.col = "x1", check.na = FALSE)[-5],
               # Not guaranteed:
               data.table(x = DT$x, x1 = dplyr::ntile(DT$x, 2))[-5])
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x1", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  DT[1L, x := 3L]
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x11", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  
})

test_that("tibble", {
  skip_on_cran()
  skip_if_not_installed("tibble")
  TIB <- tibble::tibble(hadley = 1:4)
  expect_identical(mutate_ntile(TIB, "hadley", n = 2, new.col = "hadleyTile"),
                   tibble::tibble(hadley = 1:4, hadleyTile = rep(1:2, each = 2L)))
  asf <- function(x) as.data.frame(x)
  TIB <- asf(tibble::tibble(hadley = 1:4))
  expect_identical(mutate_ntile(TIB, "hadley", n = 2, new.col = "hadleyTile"),
                   asf(tibble::tibble(hadley = 1:4, hadleyTile = rep(1:2, each = 2L))))
  
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


test_that("bys", {
  skip_on_cran()
  skip_if_not_installed("taxstats1516")
  skip_if_not_installed("dplyr")
  skip_on_circleci(1)
  
  
  
  library(data.table)
  library(hutils)
  library(magrittr)
  
  s1516 <- 
     .selector(taxstats1516::sample_file_1516_synth,
              noms = c("Gender", "Sw_amt", "Taxable_Income"))
  s1516 <- as.data.table(s1516)
  
  s1516a <- copy(s1516b <- copy(s1516))
  
  # Errors
  expect_error(mutate_ntile(s1516a, "Taxable_Income", n = 10, by = "Gender", keyby = "Gender"), 
               "`by` is NULL, yet `keyby` is NULL too. ")
  
  mutate_ntile(s1516a, "Taxable_Income", n = 100, by = "Gender")
  s1516b[, "Taxable_IncomePercentile" := dplyr::ntile(Taxable_Income, 100), by = "Gender"][]
  expect_identical(s1516a, s1516b)
  
  mutate_ntile(s1516a, "Sw_amt", n = 20, keyby = "Gender")
  s1516b <- s1516b[, "Sw_amtVigintile" := dplyr::ntile(Sw_amt, 20), keyby = "Gender"][]
  expect_identical(s1516a, s1516b)
  
  
  
  
  setkey(s1516, Taxable_Income)
  s1516a <- copy(s1516b <- copy(s1516))
  # Errors
  expect_error(mutate_ntile(s1516a, "Taxable_Income", n = 10, by = "Gender", keyby = "Gender"), 
               "`by` is NULL, yet `keyby` is NULL too. ")
  
  mutate_ntile(s1516a, "Taxable_Income", n = 100, by = "Gender")
  s1516b[, "Taxable_IncomePercentile" := dplyr::ntile(Taxable_Income, 100), by = "Gender"][]
  expect_identical(s1516a, s1516b)
  
  mutate_ntile(s1516a, "Sw_amt", n = 20, keyby = "Gender")
  s1516b <- s1516b[, "Sw_amtVigintile" := dplyr::ntile(Sw_amt, 20), keyby = "Gender"][]
  expect_identical(s1516a, s1516b)
  
  setkey(s1516, Sw_amt)
  s1516a <- copy(s1516b <- copy(s1516))
  mutate_ntile(s1516a, "Sw_amt", n = 6, keyby = "Gender")
  s1516b <- s1516b[, "Sw_amtSextile" := dplyr::ntile(Sw_amt, 6), keyby = "Gender"][]
  expect_identical(s1516a, s1516b)
  
  
  
})

