context("General inflator")

test_that("general_inflator matches cpi", {
  my_data <- 
    grattan:::cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time"))
  
  expect_equal(general_inflator(5, from = "2013-Q1", to = "2014-Q1", 
                                inflator_table = my_data), 
               cpi_inflator_quarters(5, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
  
  expect_error(general_inflator(5, from = "2013-Q1", to = "2014-Q1", 
                                inflator_table = data.table()))
})