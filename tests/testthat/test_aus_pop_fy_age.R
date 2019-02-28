context("aus_pop_fy_age")

test_that("aus pop fy age works", {
  a <- aus_pop_fy_age(tbl = TRUE)
  b <- a[fy_year >= "2005-06"][, .(Population = sum(Population)), keyby = "fy_year"]
  
  expect_true(b[, min(Population)] > 20e6)
  expect_equal(length(aus_pop_fy_age(age = 5L, fy_year = "1981-82", tbl = FALSE)), 
               1)
  expect_equal(length(aus_pop_fy_age(age = 25:26, fy_year = "2004-05", tbl = FALSE)), 
               2)
  expect_equal(length(aus_pop_fy_age(age = NULL, fy_year = "1981-82", tbl = FALSE)), 
               101)
  
})
