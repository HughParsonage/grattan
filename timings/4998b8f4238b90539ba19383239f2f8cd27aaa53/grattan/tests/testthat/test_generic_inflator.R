context("generic inflator")

test_that("Error handling", {
  expect_error(generic_inflator("MCS_Emplr_Contr", h = 1L, fy.year.of.sample.file = "2012-13"), 
               regexp = paste0("You have requested a projection ",
                               "/ inflator of 'MCS_Emplr_Contr'",
                               ", yet.*this variable was not present ",
                               "till the 2013-14 sample file."))
})

test_that("generic inflator doesn't fail!", {
  expect_is(generic_inflator(vars = "Sw_amt", h = 0L), "data.frame")
  expect_is(generic_inflator(vars = "Sw_amt", h = 1L), "data.frame")
})
  
test_that("generic inflator gives higher/lower for upper/lower", {
  expect_gte(generic_inflator(vars = "Sw_amt", h = 1L, estimator = "mean")[["inflator"]], 
             generic_inflator(vars = "Sw_amt", h = 1L, estimator = "lower")[["inflator"]])
  expect_gte(generic_inflator(vars = "MCS_Emplr_Contr", h = 1L,
                              fy.year.of.sample.file = "2013-14", nonzero = TRUE)[["inflator"]], 
             generic_inflator(vars = "MCS_Emplr_Contr", h = 1L,
                              fy.year.of.sample.file = "2013-14", nonzero = FALSE)[["inflator"]])
  
  expect_lte(generic_inflator(vars = "Sw_amt", h = 1L, estimator = "mean")[["inflator"]], 
             generic_inflator(vars = "Sw_amt", h = 1L, estimator = "upper")[["inflator"]])
})
