context("New income tax")

test_that("New tax bracket function", {
  expect_equal(new_income_tax(50e3, 
                              new_tax_tbl = data.table(fy_year = c("2013-14", "2013-14", "2013-14", "2013-14", "2013-14"), 
                                                       lower_bracket = c(0L, 18200L, 37000L, 80000L, 180000L), 
                                                       marginal_rate = c(0, 0.19, 0.325, 0.37, 0.45))), 
               7797)
  expect_error(new_income_tax(50e3, new_tax_tbl = "a"))
  expect_error(new_income_tax(50e3, new_tax_tbl = data.table(fy_year = c("2013-14", "2013-14", "2013-14", "2013-14", "2013-14"), 
                                                             foo = c(0L, 18200L, 37000L, 80000L, 180000L), 
                                                             bar = c(0, 0.19, 0.325, 0.37, 0.45))))
})


