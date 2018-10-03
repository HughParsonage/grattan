context("SAPTO")

test_that("Mising fy.year",{
  expect_warning(income_tax_sapto(33000, sapto.eligible = TRUE), 
                 regexp = "fy\\.year is missing, using current financial year")
  
  expect_equal(suppressWarnings(income_tax_sapto(33000, sapto.eligible = TRUE)),
               income_tax_sapto(33000, fy.year=date2fy(Sys.Date()), sapto.eligible = TRUE))
})

test_that("SAPTO for singles", {
  expect_equal(sapto(33279, fy.year = "2015-16"), 2105)
})

test_that("Error handling", {
  expect_error(sapto(25e3, 
                     fy.year = "2016-17", 
                     family_status = "single",
                     Spouse_income = 100),
               regexp = "Whenever `Spouse_income` is positive,")
  expect_error(sapto(rep(25e3, 2),
                     fy.year = "2016-17", 
                     family_status = rep("single", 2),
                     Spouse_income = 100),
               regexp = "Whenever `Spouse_income` is positive,")
})


test_that("SAPTO for partners", {
  skip("Govt calculator using wrong thresholds")
  expect_equal(sapto(28974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               1602)
  
  expect_equal(sapto(28974, fy.year = "2015-16", Spouse_income = 10e3, family_status = "married"), 
               2604)
  # Whoa
  expect_equal(sapto(28975, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3205)
  expect_equal(sapto(32974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204)
  expect_equal(sapto(31974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204)
  expect_equal(sapto(30974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204)
  expect_equal(sapto(29974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204)
  expect_equal(sapto(29974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               1602)
  expect_equal(sapto(28974, fy.year = "2015-16", Spouse_income = 28974, family_status = "married"), 
               1602)
  expect_equal(sapto(29974, fy.year = "2015-16", Spouse_income = 28974, family_status = "married"), 
               1477)
  
  expect_equal(sapto(57948, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               614)
  
  expect_equal(sapto(41790, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               2634)
  expect_equal(sapto(40790, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               2759)
  expect_equal(sapto(39790, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               2884)
  expect_equal(sapto(37790, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3134)
  
  expect_equal(sapto(37130, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204,
               tol = 1, scale = 1)
  expect_equal(sapto(37230, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3204, tol = 1, scale = 1)
  expect_equal(sapto(37280, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3198, tol = 1, scale = 1)
  expect_equal(sapto(37330, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               3192, tol = 1, scale = 1)
  
  
  
  expect_equal(sapto(60000, fy.year = "2015-16", Spouse_income = 100, family_status = "married"), 
               358, tol = 1, scale = 1)
  expect_equal(sapto(61000, fy.year = "2015-16", Spouse_income = 100, family_status = "married"), 
               233, tol = 1, scale = 1)
  expect_equal(sapto(62000, fy.year = "2015-16", Spouse_income = 100, family_status = "married"), 
               108, tol = 1, scale = 1)
  expect_equal(sapto(65000, fy.year = "2015-16", Spouse_income = 100, family_status = "married"), 
               0, tol = 1, scale = 1)
  
  expect_lte(abs(sapto(41000, fy.year = "2015-16", Spouse_income = 41000, family_status = "married") - 99) < 1)
  expect_lte(abs(sapto(41100, fy.year = "2015-16", Spouse_income = 41100, family_status = "married") - 87) < 1)
})

test_that("New SAPTO matches old SAPTO", {
  skip("Government calculator using wrong thresholds")
  expect_equal(new_sapto(33000, new_sapto_tbl = copy(grattan:::sapto_tbl)[fy_year == "2016-17"] %>% setkeyv("family_status")), 
               sapto(33000, "2016-17"))
  expect_equal(income_tax_sapto(33000,
                                fy.year = "2016-17",
                                sapto.eligible = TRUE,
                                new_sapto_tbl = copy(grattan:::sapto_tbl)[fy_year == "2016-17"] %>% setkeyv("family_status"),
                                medicare.sapto.eligible = TRUE), 
               income_tax(33000, "2016-17", age = 67))
})

test_that("New SAPTO matches old SAPTO for SAPTO", {
  # skip("v")
  expect_equal(new_sapto(33000, Spouse_income = 33000, family_status = "married",
                         new_sapto_tbl = copy(grattan:::sapto_tbl)[fy_year == "2016-17"] %>% setkeyv("family_status")), 
               sapto(33000, "2016-17", Spouse_income = 33000, family_status = "married"))
  
  the_sapto_tbl <- copy(grattan:::sapto_tbl)
  
  expect_equal(income_tax_sapto(33000,
                                fy.year = "2016-17",
                                sapto.eligible = TRUE,
                                new_sapto_tbl = copy(grattan:::sapto_tbl)[fy_year == "2016-17"] %>% setkeyv("family_status"),
                                medicare.sapto.eligible = TRUE), 
               income_tax(33000, "2016-17", age = 67))
  
})

test_that("income_tax_sapto", {
  expect_error(suppressWarnings(income_tax_sapto(allow.forecasts = TRUE)),
               regexp = "not used")
  skip_on_cran()
  skip_if_not_installed("taxstats", minimum_version = "0.0.5")
  library(taxstats)
  s1314 <- as.data.table(sample_file_1314)
  expect_equal(s1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = .SD)], 
               s1314[, income_tax_sapto(Taxable_Income, "2013-14", sapto.eligible = age_range <= 1, .dots.ATO = .SD)])
  expect_equal(s1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = NULL)], 
               s1314[, income_tax_sapto(Taxable_Income, "2013-14", .dots.ATO = NULL)])
  expect_equal(s1314[, income_tax(Taxable_Income, "2013-14", return.mode = "integer")],
               s1314[, income_tax_sapto(Taxable_Income, "2013-14", return.mode = "integer")])
  s1314[, Taxable_Income := as.double(Taxable_Income)]
  expect_equal(s1314[, income_tax(Taxable_Income, "2013-14", .dots.ATO = s1314)], 
               s1314[, income_tax_sapto(Taxable_Income, "2013-14", sapto.eligible = age_range <= 1, .dots.ATO = s1314)])
  
})

test_that("Works for ATO sample file", {
  # skip("v")
  skip_if_not_installed("taxstats")
  skip_if_not_installed("dplyr")
  skip_on_cran()
  library(taxstats)
  s1314 <- sample_file_1314 %>% copy
  tax_with_sapto_fn <-
    income_tax_sapto(s1314$Taxable_Income, fy.year = "2013-14",
                     .dots.ATO = copy(s1314),
                     age = if_else(s1314$age_range <= 1, 67, 42),
                     new_sapto_tbl = copy(grattan:::sapto_tbl)[fy_year == "2016-17"] %>% setkeyv("family_status"))
  tax_with_base_fn <- income_tax(s1314$Taxable_Income,
                                 fy.year = "2013-14",
                                 .dots.ATO = copy(s1314),
                                 age = if_else(s1314$age_range <= 1, 67, 42))
  
  expect_true(all(dplyr::near(tax_with_base_fn, tax_with_sapto_fn)))
  
})


