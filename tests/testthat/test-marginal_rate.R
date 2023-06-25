test_that("marginal_rate works", {
  s1314 <- .sample_file_1314()[1:101]
  mr100 <- marginal_rate(s1314, "2013-14")
  mr1   <- marginal_rate(s1314, "2013-14", run = 1)
  expect_equal(mr1[1], 0)
  expect_equal(mr100[1], 0)
})
