context("CG inflator")

test_that("CG population inflator", {
  expect_gte(grattan:::CG_population_inflator(from_fy = "2012-13", to_fy = "2013-14"), 1.1)
  expect_gte(grattan:::CG_population_inflator(from_fy = "2012-13", to_fy = "2016-17"), 1.1)
  expect_lte(grattan:::CG_inflator(from_fy = "2012-13", to_fy = "2013-14"), 1)
})


