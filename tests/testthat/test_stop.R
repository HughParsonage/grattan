context("stops (errors)")

test_that("stop_via", {
  expect_error(stop_via("qed"), "qed")
  expect_error(stop_via("qed", FUNCTION = "abc"))
  expect_match(tryCatch(stop_via("qed", FUNCTION = "abc"),
                        error = function(e) deparse(e$c)),
               "abc")
               
})
