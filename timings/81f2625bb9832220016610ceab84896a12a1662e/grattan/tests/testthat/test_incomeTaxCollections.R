library(taxstats)
library(grattan)
library(magrittr)

context("Projected tax collections")

test_that("Projections match collections", {
  collections_1314_proj.over.actual <- 
    sample_file_1213 %>%
    # ABS: 166,027 million. Cat 5506
    project_to(to_fy = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2013-14") * WEIGHT)/ (166027 * 1e6) - 1) 
  
  collections_1415_proj.over.actual <- 
    sample_file_1213 %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    project_to(to_fy = "2014-15") %$%
    abs(sum(income_tax(Taxable_Income, "2014-15") * WEIGHT) / (176600 * 1e6) - 1)
  
  expect_lt(collections_1314_proj.over.actual, 1)
  expect_lt(collections_1415_proj.over.actual, 1)
})


