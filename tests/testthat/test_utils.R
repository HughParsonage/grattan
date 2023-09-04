context("utils")

test_that("unselect_", {
  y <- data.table(x = 1, Sw_amt = 2) %>% unselect_(.dots = "Sw_amt")
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

test_that("getOption", {
  expect_equal(.getOption("grattan.sadfsdfsdfdfs", "abc"), "abc")
  expect_equal(getOption("width", "abc"), 
               .getOption("width", "abc"))
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


test_that("seq_qtr", {
  expect_equal(seq_qtr(from = "2020-Q1", length.out = 5),
               c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", 
                 "2021-Q1"))
  expect_equal(length(seq_qtr(from = "2010-Q4", length.out = 10)), 10)
})



