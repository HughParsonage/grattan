context("sample files all")

test_that("sfa", {
  library(data.table)
  
  library(magrittr)
  expect_true(TRUE)
  sfa <- .sfa()[, "fy_year" := .SD[["fy.year"]]][, WEIGHT := fifelse(fy_year <= "2011-12", 100L, 50L)]
  
  sfa200304 <- 
    sfa[fy_year == "2003-04"] %>%
    .[, tax := income_tax(Taxable_Income, "2003-04", .dots.ATO = .SD)]

  
  s0304 <- copy(.sample_file_("0304"))
  s0304[, tax := income_tax(Taxable_Income, "2003-04", .dots.ATO = .SD)]
  
  
  expect_equal(s0304[["Ind"]], sfa200304[["Ind"]]) # Just to check order-preservation
  expect_equal(s0304[["tax"]], sfa200304[["tax"]])
  
  sfa_201213 <- sfa[fy_year == "2012-13"]
  sfa_201213[, tax := income_tax(Taxable_Income, "2012-13", .dots.ATO = .SD)]
  
  sfa[, tax1 := income_tax(Taxable_Income, fy.year = fy_year, .dots.ATO = sfa)]
  sfa[, tax2 := income_tax(Taxable_Income, fy.year = .BY[[1]], .dots.ATO = .SD), by = "fy_year"]
  expect_equal(sfa$tax1, sfa$tax2)
  
  tot_tax_by_year <- 
    sfa[, .(tax = sum(tax1), wt = first(WEIGHT)), keyby = "fy_year"][, tax := wt * tax]
  
  # In case there are integer NAs
  expect_lt(tot_tax_by_year[.("2010-11"), tax], 150e9)
  
  # Plausible but not verified numbers
  tot_tax_by_year[fy_year == "2003-04", expected_tax := 99e9]
  tot_tax_by_year[fy_year == "2004-05", expected_tax := 106e9]
  tot_tax_by_year[fy_year == "2005-06", expected_tax := 113e9]
  tot_tax_by_year[fy_year == "2006-07", expected_tax := 120e9]
  tot_tax_by_year[fy_year == "2010-11", expected_tax := 120e9]
  
  max_err <- tot_tax_by_year[, max(expected_tax - tax, na.rm = TRUE)]
  expect_lt(max_err, 5e9)
  
  
})


