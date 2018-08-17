context("stops (errors)")

test_that("stop_via", {
  expect_error(stop_via("qed"), "qed")
  invisible(expect_error(stop_via("qed", FUNCTION = "abc"), "Error in abc: qed", fixed = TRUE))
  
})
