context("Aus pop qtr")

test_that("Returns correct values", {
  expect_equal(round(aus_pop_qtr("2000-Q1", allow.projections = FALSE, fertility = "high", mortality = "high.LifeExpectancy"), -4), 
               18990000)
  
  x <- aus_pop_qtr("2030-Q1", allow.projections = TRUE, fertility = "high")
  y <- aus_pop_qtr("2030-Q1", allow.projections = TRUE, fertility = "medium")
  z <- aus_pop_qtr("2030-Q1", allow.projections = TRUE, fertility = "low")
  
  expect_gte(x, 
             y)
  
  expect_lte(z, 
             y)
  
  expect_warning(aus_pop_qtr("2030-Q1", allow.projections = FALSE, fertility = "high"))
  
  # Assumes population continues to grow!
  expect_gte(suppressWarnings(aus_pop_qtr("2030-Q1", allow.projections = FALSE, fertility = "high")), 
             24e6)
  
})
