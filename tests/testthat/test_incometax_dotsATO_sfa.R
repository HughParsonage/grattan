context("sample files all")

test_that("sfa", {
  skip_if_not_installed("taxstats", minimum_version = "0.1.0.1415")
  library(data.table)
  library(taxstats)
  library(magrittr)
  sfa <- get_sample_files_all2()
  
  sfa200304 <- 
    sfa[fy_year == "2003-04"] %>%
    .[, tax := income_tax(Taxable_Income, "2003-04", .dots.ATO = .SD)]

  
  s0304 <- copy(sample_file_0304)
  s0304[, tax := income_tax(Taxable_Income, "2003-04", .dots.ATO = .SD)]
  
  
  expect_equal(s0304[["Ind"]], sfa200304[["Ind"]]) # Just to check order-preservation
  expect_equal(s0304[["tax"]], sfa200304[["tax"]])
  
  sfa_201213 <- sfa[fy_year == "2012-13"]
  sfa_201213[, tax := income_tax(Taxable_Income, "2012-13", .dots.ATO = .SD)]
  
  
})


