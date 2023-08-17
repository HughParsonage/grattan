context("Benchmarks")

test_that("Performance regression: cpi_inflator", {
  skip_on_cran()
  skip_if_not(identical(.Platform$r_arch, "x64"))
  OR <- `||`
  AND <- `&&`
  skip_if_not(OR(hughs_computer <- identical(Sys.getenv("USERNAME"), "hughp"),
                 AND(AND(identical(Sys.getenv("TRAVIS"), "true"), 
                         identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "devel")),
                     Sys.getenv("TRAVIS_PULL_REQUEST") != "true")))
  
  set.seed(19842014)
  from_fys10K <- sample(yr2fy(1984:2014), size = 10e3, replace = TRUE)
  from_fys100M <- rep(from_fys10K, times = 100e6/10e3)
  cpi_infl_time100K <- system.time(cpi_inflator(from = from_fys10K,
                                                to = "2015-16", 
                                                series = "original"))
  cpi_infl_time100M <- system.time(cpi_inflator(from = from_fys100M,
                                                to = "2015-16",
                                                series = "original"))
  expect_lt(cpi_infl_time100M[["elapsed"]], 
            if (hughs_computer) 5 else 17)
})

test_that("Performance regression: wage_inflator", {
  skip_on_cran()
  skip_if_not(identical(.Platform$r_arch, "x64"))
  OR <- `||`
  AND <- `&&`
  skip_if_not(OR(hughs_computer <- identical(Sys.getenv("USERNAME"), "hughp"),
                 AND(AND(identical(Sys.getenv("TRAVIS"), "true"), 
                         identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "devel")),
                     Sys.getenv("TRAVIS_PULL_REQUEST") != "true")))
  set.seed(19992014)
  from_fys10K <- sample(yr2fy(1999:2014), size = 10e3, replace = TRUE)
  from_fys100M <- rep(from_fys10K, times = 100e6/10e3)
  wage_infl_time10K <- system.time(wage_inflator(from = from_fys10K,
                                                 to = "2015-16"))
  wage_infl_time100M <- system.time(wage_inflator(from = from_fys100M,
                                                  to = "2015-16"))
  expect_lt(wage_infl_time10K[["elapsed"]], 
            if (hughs_computer) 0.2 else 0.5)
  expect_lt(wage_infl_time100M[["elapsed"]], 
            if (hughs_computer) 5 else 17)
})

