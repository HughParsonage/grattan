context("print_2heading_xtable")

test_that("print_2heading_xtable", {
  .data1 <- data.table(x = 1, y = 2)
  .data2 <- data.table(x__a = 1, x__b = 5, y = 2)
  
  expect_error(print_2heading_xtable(.data = .data))
  expect_error(print_2heading_xtable(.data = .data, include.colnames = FALSE))
  example_df <- 
    data.frame(yr = 2001:2005, 
               Revenue__foo = 1:5, 
               Revenue__bar = 11:15, 
               Revenue__baz = 21:25, 
               ordinary = 1:5,
               Expense__foo = 1:5,
               Expense__bar = 11:15, 
               Expense__baz = 21:25, 
               Last__foo = 1:5, 
               Last__baz = 2:6,
               last = 101:105)
})

