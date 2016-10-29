context("(General) Inflator")

test_that("inflator matches cpi", {
  my_data <- 
    grattan:::cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time"))
  
  expect_equal(inflator(5, from = "2013-Q1", to = "2014-Q1", 
                                inflator_table = my_data), 
               cpi_inflator_quarters(5, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
  
  expect_error(inflator(5, from = "2013-Q1", to = "2014-Q1", 
                                inflator_table = data.table()))
})

test_that("Switching order of to and from causes inverse", {
  my_data <- 
    grattan:::cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time"))
  
  expect_equal(inflator(5, to = "2013-Q1", from = "2014-Q1", 
                        inflator_table = my_data), 
               5 / cpi_inflator_quarters(1, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
})

test_that("rolling inflator", {
  my_data <- 
    grattan:::cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time")) %>%
    mutate(Time = as.Date.yearqtr(as.yearqtr(Time, format = "%Y-Q%q"))) %>% .[]
  
  expect_equal(inflator(5, 
                        to = as.Date("2013-01-02"), 
                        from = as.Date("2014-01-01"), 
                        inflator_table = my_data, 
                        roll = Inf), 
               5 / cpi_inflator_quarters(1, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
})

