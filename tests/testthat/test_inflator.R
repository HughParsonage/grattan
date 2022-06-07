context("(General) Inflator")

test_that("inflator matches cpi", {
  my_data <- 
    cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time"))
  
  expect_equal(inflator(5, from = "2013-Q1", to = "2014-Q1", 
                                inflator_table = my_data), 
               cpi_inflator_quarters(5, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
})

test_that("Switching order of to and from causes inverse", {
  my_data <- 
    cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time"))
  
  expect_equal(inflator(5, to = "2013-Q1", from = "2014-Q1", 
                        inflator_table = my_data), 
               5 / cpi_inflator_quarters(1, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
})

test_that("rolling inflator", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("zoo")
  my_data <- 
    cpi_seasonal_adjustment %>% 
    data.table::copy(.) %>%
    setnames(old = c("obsValue", "obsTime"), 
             new = c("Index", "Time")) %>%
    .[, Time := zoo::as.Date.yearqtr(zoo::as.yearqtr(Time, format = "%Y-Q%q"))]
  
  expect_equal(inflator(5, 
                        to = as.Date("2013-01-02"), 
                        from = as.Date("2014-01-01"), 
                        inflator_table = my_data, 
                        roll = Inf), 
               5 / cpi_inflator_quarters(1, from_qtr = "2013-Q1", to_qtr = "2014-Q1"))
})

test_that("When to is long", {
  # Issue #124
  to_fy <- c("2015-16", "2017-18", "2016-17")
  res1 <- wage_inflator(1, from_fy = "2014-15", to_fy = to_fy)
  res2 <- wage_inflator(1, from_fy = rep("2014-15", 3), to_fy = to_fy)
  expect_equal(res1, res2)
})

test_that("Reversible (to < from)", {
  x <- 2.5
  o <- lf_inflator_fy(x,
                      from_fy = "2015-16",
                      to_fy = c("2011-12", "2014-15", "2013-14"))
  expect_equal(lf_inflator_fy(o,
                              to_fy = "2015-16",
                              from_fy = c("2011-12", "2014-15", "2013-14")),
               rep_len(2.5, 3))
  x <- 2.5
  o <- cpi_inflator(x,
                    from_fy = "2015-16",
                    to_fy = c("2011-12", "2014-15", "2013-14"))
  expect_equal(cpi_inflator(o,
                            to_fy = "2015-16",
                            from_fy = c("2011-12", "2014-15", "2013-14")),
               rep_len(2.5, 3))
  
  x <- 2.5
  o <- wage_inflator(x,
                     from_fy = "2015-16",
                     to_fy = c("2011-12", "2014-15", "2013-14"))
  expect_equal(wage_inflator(o,
                             to_fy = "2015-16",
                             from_fy = c("2011-12", "2014-15", "2013-14")),
               rep_len(2.5, 3))
})

