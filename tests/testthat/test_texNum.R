
context("texNum return correct")
test_that("texNum returns known results", {
  expect_equal(texNum(180000), "180,000")
  expect_equal(texNum(1800000), "1.8~million")
  expect_equal(texNum(1850000), "1.85~million")
  expect_equal(texNum(1850000, 2), "1.8~million")
  expect_equal(texNum(1850000, 2, TRUE), "\\$1.8~million")
  expect_equal(texNum(-1850000, 2, TRUE), "$-$\\$1.8~million")
  expect_equal(texNum(-5), "$-$5")
  expect_equal(texNum(500e6 - 1, pre.phrase = c("almost", "over")), "almost 0.5~billion")
  expect_equal(texNum(500e6 + 1, pre.phrase = c("almost", "over")), "over 0.5~billion")
})
