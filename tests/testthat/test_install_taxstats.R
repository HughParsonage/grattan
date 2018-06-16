context("install_taxstats")

test_that("Installs", {
  skip_on_cran()
  tempf <- tempfile()
  dir.create(tempf)
  install_taxstats(lib = tempf)
})
