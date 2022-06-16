test_that("Validation of System", {
  Sys <- System(2018L, 
                sapto_lower_threshold = 30e3, 
                sapto_max_offset = 2000)
  expect_equal(Sys$Sapto$mxo_single, 2000)
  
})