
context("texNum return correct")
test_that("texNum returns known results", {
  expect_equal(texNum(180000), "180,000")
  expect_equal(texNum(1800000), "1.8~million")
  expect_equal(texNum(1850000), "1.85~million")
  expect_equal(texNum(1850000, 2), "1.8~million")
  expect_equal(texNum(1850000, 2, TRUE), "\\$1.8~million")
  expect_equal(texNum(-1850000, 2, TRUE), "\\(-\\)\\$1.8~million")
  expect_equal(texNum(-5), "\\(-\\)5")
  expect_equal(texNum(500e6 - 1, pre.phrase = c("almost", "over")), "almost 0.5~billion")
  expect_equal(texNum(500e6, pre.phrase = c("almost", "over")), "0.5~billion")
  expect_equal(texNum(500e6 + 1, pre.phrase = c("almost", "over")), "over 0.5~billion")
  expect_true(texNum(380e9) %in% c("380~billion", "0.38~trillion"))
  expect_true(texNum(380e9 - 1, pre.phrase = c("X", "Y")) %in% c("X 380~billion", "X 0.38~trillion"))
  expect_true(texNum(380e6) %in% c("380~million", "0.38~billion"))
  expect_equal(texNum(1.25, dollar = TRUE), "\\$1.25")
})

context("grattan_dollar")

test_that("grattan_dollar correct", {
  expect_equal(grattan_dollar(100), "$100")
  expect_equal(grattan_dollar(-100), paste0("\U2212", "$100"))
})

context("Small numbers correct")

test_that("grattan_percent correct", {
  expect_equal(grattan_percent(0.45, .percent.suffix = " per cent"), "45 per cent")
})


test_that("Error handling", {
  expect_error(Grattan_frac(1.1))
})

test_that("Ggrattan_frac correct answers", {
  expect_equal(Grattan_frac(0.51), "Over one-half") 
  expect_equal(Grattan_frac(0.49), "Almost one-half") 
  expect_equal(Grattan_frac(0.51, NULL), "One-half") 
  expect_equal(Grattan_frac(0.49, NULL), "One-half") 
  expect_equal(grattan_frac(0.49, NULL), "one-half") 
})
