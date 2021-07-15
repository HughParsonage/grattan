test_that("CJ model", {
  skip_on_cran()
  skip_if_not_installed("data.table")
  library(data.table)
  
  # Commenting out because it takes too long
  # only leaving in previously errored
  
  # scj <- 
  #   CJ(Taxable_Income = 1:10e3,
  #      age_range = 1:2,
  #      Spouse_adjusted_taxable_inc = 0:30e3)
  # scj[, Partner_status := as.integer(as.logical(Spouse_adjusted_taxable_inc))] 
  # scj[, original := as.integer(income_tax(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  # scj[, new_tax := as.integer(income_tax2(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  # 
  # w0 <- scj[, which_first(original != new_tax)]
  # expect_equal(w0, 0)
  # 
  # scj <- 
  #   CJ(Taxable_Income = 10e3:20e3,
  #      age_range = 1:2,
  #      Spouse_adjusted_taxable_inc = 0:30e3)
  # scj[, Partner_status := as.integer(as.logical(Spouse_adjusted_taxable_inc))]
  # scj[, original := as.integer(income_tax(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  # scj[, new_tax := as.integer(income_tax2(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  # w0 <- scj[, which_first(original != new_tax)]
  # expect_equal(w0, 0)
  
  scj <- 
    CJ(Taxable_Income = 20e3:30e3,
       age_range = 1:2,
       Spouse_adjusted_taxable_inc = 0:30e3)
  scj[, Partner_status := as.integer(as.logical(Spouse_adjusted_taxable_inc))]
  scj[, original := as.integer(income_tax(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  scj[, new_tax := as.integer(income_tax2(Taxable_Income, fy.year = "2016-17", .dots.ATO = .SD))]
  w0 <- scj[, which_first(original != new_tax)]
  expect_equal(w0, 0)
  scj <- NULL
  
})
  