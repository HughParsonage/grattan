test_that("cached law tables agree with the authoritative source tables", {
  root <- testthat::test_path("..", "..")
  loader <- file.path(root, "data-raw", "update-law-data.R")
  skip_if_not(file.exists(loader), "Source-only law-table inputs are unavailable")
  helpers <- new.env(parent = baseenv())
  sys.source(loader, envir = helpers)
  expected <- helpers$read_law_tables(root)
  cached <- new.env(parent = baseenv())
  load(file.path(root, "R", "sysdata.rda"), envir = cached)
  for (name in names(expected)) {
    expect_equal(cached[[name]], expected[[name]], info = name)
    expect_identical(data.table::key(cached[[name]]),
                     data.table::key(expected[[name]]), info = name)
  }
})

test_that("law-data cache includes the corrected historical and enacted schedules", {
  # These independent legal values guard the data rebuild as well as C tables.
  expect_equal(grattan:::tax_table2[fy_year == "2026-27" & income == 45000, tax_at],
               4020)
  expect_equal(grattan:::lito_tbl[fy_year == "2002-03", min_bracket], 20700)
  expect_equal(grattan:::lito_tbl[fy_year == "2020-21", max_lito], 700)
  expect_equal(grattan:::lito_tbl[fy_year == "2020-21", second_bracket], 45000)
  expect_equal(grattan:::medicare_tbl[fy_year == "2024-25" & !sapto & !pto,
                                     lower_threshold], 27222)
  expect_equal(grattan:::sapto_tbl[fy_year == "2024-25" & family_status == "single",
                                  lower_threshold], 34919)
})
