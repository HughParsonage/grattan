library(taxstats)
library(grattan)
library(magrittr)

context("Projected tax collections")

test_that("Projections match collections", {
  collections_1314_proj.over.actual <- 
    sample_file_1314 %>%
    # ABS: 166,027 million. Cat 5506
    project_to(to_fy = "2013-14", fy.year.of.sample.file = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2013-14") * WEIGHT)/ (166027 * 1e6) - 1) 
  
  collections_1415_proj.over.actual <- 
    sample_file_1314 %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    project_to(to_fy = "2014-15", fy.year.of.sample.file = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2014-15") * WEIGHT) / (176600 * 1e6) - 1)
  
  # http://budget.gov.au/2016-17/content/bp1/download/bp1.pdf
  # Table 7
  collections_1516_proj.over.actual <- 
    sample_file_1314 %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    project_to(to_fy = "2015-16", fy.year.of.sample.file = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2015-16") * WEIGHT) / (188400 * 1e6) - 1)
  
  # http://budget.gov.au/2016-17/content/bp1/download/bp1.pdf
  # Table 7
  collections_1617_proj.over.actual <- 
    sample_file_1314 %>%
    # Budget papers http://www.budget.gov.au/2015-16/content/bp1/html/bp1_bs4-03.htm
    project_to(to_fy = "2016-17", fy.year.of.sample.file = "2013-14") %$%
    abs(sum(income_tax(Taxable_Income, "2016-17") * WEIGHT) / (196950 * 1e6) - 1)
  
  expect_lt(collections_1314_proj.over.actual, 0.05)
  expect_lt(collections_1415_proj.over.actual, 0.05)
  expect_lt(collections_1516_proj.over.actual, 0.05)
  expect_lt(collections_1617_proj.over.actual, 0.05)
})


