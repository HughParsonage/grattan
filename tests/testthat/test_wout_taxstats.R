context("Tests to deploy when taxstats not present")

test_that("mutate_super_vars", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("TRAVIS"), "true"))
  skip_if_not(identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release"))
  skip_if_not_installed("taxstats")
  library(utils)
  
  if (requireNamespace("taxstats", quietly = TRUE)) {
    s1314 <- as.data.table(taxstats::sample_file_1314)
    res <- apply_super_caps_and_div293(s1314)
    if ("taxstats" %in% .packages()) {
      detach("package:taxstats", unload = TRUE)
    } else if (isNamespaceLoaded("taxstats")) {
      unloadNamespace("taxstats")
    }
    remove.packages("taxstats")
  }
  
  # age range decoder
  res_wout_taxstats <- apply_super_caps_and_div293(s1314)
  expect_equal(res_wout_taxstats, res)
  
  
  
  
})
