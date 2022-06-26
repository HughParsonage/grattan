context("utils")

test_that("unselect_", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  y <- sample_file_1314 %>% copy %>% unselect_(.dots = "Sw_amt")
  expect_false("Sw_amt" %in% names(y))
})

test_that("as.numeric_unless_warning", {
  x <- c("1", "2", "3")
  y <- c("1", "2", "foo")
  expect_equal(as.numeric_unless_warning(x), 1:3)
  expect_equal(as.numeric_unless_warning(y), y)
})

test_that("anyIntersection", {
  expect_true(anyIntersection(1:5, 5:10))
  expect_false(anyIntersection(NA, FALSE))
  expect_false(anyIntersection(letters[1:5], LETTERS[1:5]))
})

test_that("last_over_first", {
  expect_equal(last_over_first(1:5), 5L)
  expect_equal(last_over_first(letters[1:5]), letters[1:5])
  expect_true(all(are_zero(c(0, 0.1 + 0.2 - 0.3))))
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
  expect_error(prohibit_vector_recycling.MAXLENGTH(c(2, 2), 1, c(3, 3, 3)))
  expect_equal(prohibit_vector_recycling.MAXLENGTH(c(2, 2), 1:2, 1), 2L)
  expect_equal(prohibit_arg_recycling.MAXLENGTH(list(c(2, 2), 1:2, 1)), 2L)
  expect_error(prohibit_arg_recycling.MAXLENGTH(list(a = 1:3, b = 1:2, d = 1)))
})

test_that("qtrs_ahead", {
  expect_equal(qtrs_ahead("2016-Q1", "2017-Q1"), 4)
  expect_equal(qtrs_ahead("2016-Q1", "2016-Q3"), 2)
  expect_equal(qtrs_ahead("2016-Q1", "2017-Q2"), 5)
})

test_that("fast selector", {
  library(data.table)
  dt <- data.table(x = 1:5, y = 11:15, z = letters[1:5], key = "z")
  expect_identical(.selector(dt, noms = c("z", "y")), 
                   dt[, .(z, y)])
})

test_that("koffset", {
  expect_equal(koffset(37000 + 1:10, c(0, 37e3, 66667, Inf), c(445, 445, 0, 0)), 
               lito(37000 + 1:10, "2015-16"))
})

test_that("Switch", {
  expect_equal(Switch(c("A", "B", "C", "A"), "A" = 1, "B" = 2, "C" = 11:14 + 0, DEFAULT = 0), 
               c(1, 2, 13, 1))
  expect_equal(Switch(c("A", "B", "C", "A"), "A" = 1:4, "B" = 2, "C" = 11:14 + 0, DEFAULT = 0), 
               c(1, 2, 13, 4))
})

test_that("Accelerate inputs", {
  skip_on_cran()
  set.seed(3713907)
  yrs <- sample(1990:2010, size = 1e5, replace = TRUE)
  fys <- yr2fy(yrs)
  expect_identical(fys, 
                   accel_repetitive_input(yrs, "yr2fy"))
  expect_identical(fys, 
                   accel_repetitive_input(yrs, yr2fy, THRESHOLD = 10L))
  cpi15 <- function(x) {
    cpi_inflator(from_fy = x, to_fy = "2015-16", adjustment = "none")
  }
  expect_identical(cpi_inflator(from_fy = fys, to_fy = "2015-16", adjustment = "none"), 
                   accel_repetitive_input(fys, cpi15))
  
  expect_identical(cpi_inflator(from_fy = c("2015-16", "2015-16", "2015-16"), 
                                to_fy = "2016-17"),
                   accel_repetitive_input(c("2015-16", "2015-16", "2015-16"),
                                          FUN = cpi_inflator,
                                          from_nominal_price = 1,
                                          to_fy = "2016-17",
                                          THRESHOLD = 2L))
  
  y <- runif(1001)
  expect_identical(accel_repetitive_input(y, log), 
                   accel_repetitive_input(y, log, THRESHOLD = 2000L))
  expect_identical(accel_repetitive_input(2, log), 
                   log(2))
  
})

test_that("getOption", {
  expect_equal(.getOption("grattan.sadfsdfsdfdfs", "abc"), "abc")
  expect_equal(getOption("width", "abc"), 
               .getOption("width", "abc"))
})

test_that("get_qtr", {
  skip_if_not_installed("zoo")
  DT2017 <- data.table(Dates = seq.Date(as.Date(c("2017-01-01")),
                                        as.Date(c("2017-12-31")), 
                                        by = "1 day"))
  DT2017[, ZooQtr := zoo::as.yearqtr(Dates)]
  DT2017[, GratQtr := get_qtr(Dates)]
  DT2017z <- unique(DT2017, by = "ZooQtr")
  DT2017g <- unique(DT2017, by = "GratQtr")
  expect_equal(DT2017g, DT2017z)
  
  DT2017 <- DT2017z <- DT2017g <- NULL # to avoid continuation
  # leap years
  DT2000 <- data.table(Dates = seq.Date(as.Date(c("2000-01-01")),
                                        as.Date(c("2000-12-31")), 
                                        by = "1 day"))
  DT2000[, ZooQtr := zoo::as.yearqtr(Dates)]
  DT2000[, GratQtr := get_qtr(Dates)]
  DT2000z <- unique(DT2000, by = "ZooQtr")
  DT2000g <- unique(DT2000, by = "GratQtr")
  expect_equal(DT2000g, DT2000z)
})

test_that("age2age_range", {
  expect_equal(age2age_range(65), 1)
  expect_equal(age2age_range(64), 2)
  expect_equal(age2age_range(60), 2)
  expect_equal(age2age_range(24), 10)
  expect_equal(age2age_range(20), 10)
  expect_equal(age2age_range(19), 11)
})

test_that("hasntName", {
  expect_true(hasntName(data.table(x = 1), "y"))
  expect_false(hasntName(data.table(x = 1), "x"))
})


test_that("seq.qtr", {
  expect_equal(seq.qtr(from = "2020-Q1", length.out = 5),
               c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", 
                 "2021-Q1"))
  expect_equal(length(seq.qtr(from = "2010-Q4", length.out = 10)), 10)
})



