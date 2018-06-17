context("Differential uprating")


test_that("Error handling", {
  expect_error(differentially_uprate_wage(from_fy = "2016-17", to_fy = "2017-18"))
})

test_that("Differential uprate factor preserves order", {
  x <- differentially_uprate_wage(c(0, 100e3), from_fy = "2013-14", to_fy = "2014-15")
  
  expect_lt(x[1], x[2])
})

test_that("Wage growth is higher for extreme salaries", {
  skip_on_cran()
  skip_if_not_installed("taxstats")
  skip_if_not_installed("dplyr")
  library(taxstats)
  library(dplyr)
  skip_if_not(exists("get_sample_files_all"))
  try(sample_files_all <- get_sample_files_all())
  skip_if_not(exists("sample_files_all"))
  
  extreme_tile <- sample(c(1:20, 80:100), size = 1)
  
  # Not comparing 20 with 79 or 21 with 80
  if (extreme_tile < 50){
    moderate_tile <- sample(21:49, size = 1)
  } else {
    moderate_tile <- sample(51:79, size = 1)
  }
  
  from_year <- sample(2004:2014, size = 1)
  to_year <- from_year + rpois(1, 4) + 1
  
  from_fy <- yr2fy(from_year)
  to_fy <- yr2fy(to_year)
  
  invisible(sample_files_all)
  
  salaries <- 
    sample_files_all %>%
    select(fy.year, Sw_amt) %>%
    filter(fy.year == from_fy, Sw_amt > 0) %>%
    mutate(tile = ntile(Sw_amt, 100))
  
  extreme_salary <- 
    salaries %>%
    filter(tile == extreme_tile) %$%
    sample(Sw_amt, size = 1)
  
  moderate_salary <- 
    salaries %>%
    filter(tile == moderate_tile) %$%
    sample(Sw_amt, size = 1)
  
  basic_inflation <- wage_inflator(c(extreme_salary, moderate_salary), from_fy = from_fy, to_fy = to_fy)
  diffe_inflation <- differentially_uprate_wage(c(extreme_salary, moderate_salary), from_fy = from_fy, to_fy = to_fy)
  
  expect_true(diff(diffe_inflation / basic_inflation) <= 0, 
              info = as.character(
                "extreme_tile = ", extreme_tile, "\n",
                "moderate_tile = ", moderate_tile, "\n",
                "from_fy = ", from_fy, "\n",
                "to_fy = ", to_fy, "\n",
                "extreme_salary = ", extreme_salary, "\n",
                "moderate_salary = ", moderate_salary, "\n"
              ))
  
})

test_that("Differentially uprated wage growth is *up*", {
  from_year <- sample(2004:2014, size = 1)
  to_year <- from_year + rpois(1, 4) + 1
  
  from_fy <- yr2fy(from_year)
  to_fy <- yr2fy(to_year)
  
  salary <- (abs(rlnorm(1, 11, 1)) + 1)
  
  expect_true(differentially_uprate_wage(salary, from_fy = from_fy, to_fy = to_fy) > salary, 
              info = as.character(c("\n",
                "salary = ", salary, "\n",
                "from_fy = ", from_fy,  "\n",
                "to_fy = ", to_fy, "\n"
              )))
})

test_that("Less than 0.1% of individuals move more than one percentile over 10 years", {
  skip_if_not_installed("taxstats") 
  skip_on_cran()
  prop_move <- 
    sample_file_1314 %>%
    select(Sw_amt) %>%
    mutate(percentile = ntile(Sw_amt, 100)) %>%
    mutate(Sw_amt_2324 = differentially_uprate_wage(Sw_amt, "2013-14", "2023-24"), 
           percentile_2324 = ntile(Sw_amt_2324, 100)) %$%
    mean(percentile != percentile_2324)
  
  expect_lt(prop_move, 0.001)
})

test_that("differential wage inflator is mean-preserving", {
  skip_if_not_installed("taxstats") 
  skip_on_cran()
  library(magrittr)
  salaries_1314 <- sample_file_1314$Sw_amt

  salaries_1314_vanilla <- wage_inflator(salaries_1314, from_fy = "2013-14", to_fy = "2015-16")
  salaries_1314_differe <- differentially_uprate_wage(salaries_1314, from_fy = "2013-14", to_fy = "2015-16")
  
  t_test <- 
    stats::t.test(salaries_1314_vanilla, salaries_1314_differe, conf.level = 0.66) %>%
    use_series("p.value")

  expect_gt(t_test, 0.05)
})
