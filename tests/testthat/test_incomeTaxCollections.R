context("Tax collections")

test_that("income_tax collections in 2003-04 match final budget outcome by 1%", {
  skip_if_not_installed("taxstats", minimum_version = package_version("0.0.5")) 
  skip_on_cran()
  skip_on_appveyor()
  skip_on_circleci(2)
  library(taxstats)
  # http://www.budget.gov.au/2003-04/fbo/download/FBO_2003_04.pdf
  final_budget_outcome_0304 <- 98.779 * 1e9
  collections_0304 <- sum(income_tax(sample_file_0304[["Taxable_Income"]], 
                                     fy.year = "2003-04", 
                                     .dots.ATO = copy(sample_file_0304), 
                                     age = if_else(sample_file_0304$Birth_year < 2, 67, 42))) * 100 
  
  expect_lt(abs(collections_0304 - final_budget_outcome_0304) / final_budget_outcome_0304, 
            0.01)
})

test_that("income_tax collections in 2006-07 match final budget outcome by 1%", {
  skip_if_not_installed("taxstats", minimum_version = package_version("0.0.5")) 
  skip_on_cran()
  skip_on_appveyor()
  skip_on_circleci(2)
  library(taxstats)
  # http://www.budget.gov.au/2006-07/fbo/download/FBO_2006-07.pdf
  final_budget_outcome_0607 <- 117.614 * 1e9
  collections_0607 <- sum(income_tax(sample_file_0607[["Taxable_Income"]], 
                                     fy.year = "2006-07", 
                                     .dots.ATO = copy(sample_file_0607), 
                                     age = if_else(sample_file_0607$Birth_year < 2, 67, 42))) * 100 
  
  expect_lt(abs(collections_0607 - final_budget_outcome_0607) / final_budget_outcome_0607, 
            0.01)
})


test_that("Projections match collections", {
  skip_if_not_installed("taxstats", minimum_version = package_version("0.0.5")) 
  skip_on_cran()
  skip_on_appveyor()
  skip_on_circleci(2)
  library(taxstats)
  library(magrittr)
  library(data.table)
  collections_1314_proj.over.actual <- 
    sample_file_1213 %>%
    # ABS: 166,027 million. Cat 5506
    project_to(to_fy = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2013-14", 
                       age = if_else(age_range <= 1, 67, 42), 
                       .dots.ATO = copy(.)) * WEIGHT)/ (166027 * 1e6) - 1) 
  
  collections_1415_proj.over.actual <- 
    sample_file_1415_synth %$%
    abs(sum(income_tax(Taxable_Income, 
                       "2014-15", 
                       age = if_else(age_range <= 1, 67, 42), 
                       .dots.ATO = copy(.)) * 50) / (176600 * 1e6) - 1)
  
  # http://budget.gov.au/2016-17/content/bp1/download/bp1.pdf
  # Table 7
  collections_1516_proj.over.actual <- 
    sample_file_1415_synth %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    project_to(to_fy = "2015-16") %$%
    abs(sum(income_tax(Taxable_Income, 
                       "2015-16", 
                       age = if_else(age_range <= 1, 67, 42), 
                       .dots.ATO = copy(.)) * WEIGHT) / (188400 * 1e6) - 1)
  
  # http://budget.gov.au/2016-17/content/bp1/download/bp1.pdf
  # Table 7
  collections_1617_proj.over.actual <- 
    sample_file_1415_synth %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm 196950M
    # FBO: 193863M
    project_to(to_fy = "2016-17") %>%
    .[, tax := income_tax(Taxable_Income, 
                          "2016-17", 
                          age = if_else(age_range <= 1, 67, 42), 
                          .dots.ATO = copy(.), .debug = TRUE)] %>%
    .[, .(total = sum(tax * WEIGHT))] %$%
    total %>%
    divide_by(193863 * 1e6) %>%
    subtract(1)
  
  expect_lt(collections_1314_proj.over.actual, 0.05)
  expect_lt(collections_1415_proj.over.actual, 0.05)
  expect_lt(collections_1516_proj.over.actual, 0.05)
  expect_lt(collections_1617_proj.over.actual, 0.05)
})


