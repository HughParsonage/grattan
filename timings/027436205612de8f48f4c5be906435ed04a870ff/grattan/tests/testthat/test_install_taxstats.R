context("install_taxstats")

test_that("Installs", {
  skip_on_cran()
  skip_if_not(identical("true", Sys.getenv("TRAVIS")))
  skip_on_travis()
  skip_if_not(identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release"))
  tempf <- tempfile()
  dir.create(tempf)
  expect_null(install_taxstats(lib = tempf))
})
