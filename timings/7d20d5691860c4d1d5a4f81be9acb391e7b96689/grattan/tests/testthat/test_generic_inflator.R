context("generic inflator")

test_that("generic inflator doesn't fail!", {
  expect_is(generic_inflator(vars = "Sw_amt", h = 0L), "data.frame")
  expect_is(generic_inflator(vars = "Sw_amt", h = 1L), "data.frame")
})
