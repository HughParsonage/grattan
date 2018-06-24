context("generic inflator")

test_that("generic inflator doesn't fail!", {
  expect_is(generic_inflator(vars = "Sw_amt", h = 0L), "data.frame")
  expect_is(generic_inflator(vars = "Sw_amt", h = 1L), "data.frame")
})
  
test_that("generic inflator gives higher/lower for upper/lower", {
  expect_gte(generic_inflator(vars = "Sw_amt", h = 1L, estimator = "mean")[["inflator"]], 
             generic_inflator(vars = "Sw_amt", h = 1L, estimator = "lower")[["inflator"]])
  
  expect_lte(generic_inflator(vars = "Sw_amt", h = 1L, estimator = "mean")[["inflator"]], 
             generic_inflator(vars = "Sw_amt", h = 1L, estimator = "upper")[["inflator"]])
})
