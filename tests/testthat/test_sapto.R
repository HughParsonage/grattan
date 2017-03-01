context("SAPTO")

test_that("SAPTO for singles", {
  sapto(33279, fy.year = "2015-16")
})


test_that("SAPTO for partners", {
  expect_equal(sapto(28974, fy.year = "2015-16", Spouse_income = 1e-6, family_status = "married"), 
               1602)
  expect_equal(sapto(28974, fy.year = "2015-16", Spouse_income = 28974, family_status = "married"), 
               1602)
  expect_equal(sapto(29974, fy.year = "2015-16", Spouse_income = 28974, family_status = "married"), 
               1477)
})

test_that("New SAPTO matches old SAPTO", {
  expect_equal(new_sapto(33000, new_sapto_tbl = copy(grattan:::sapto_tbl) %>% filter(fy_year == "2016-17") %>% setkeyv("family_status")), 
               sapto(33000, "2016-17"))
  expect_equal(income_tax_sapto(33000,
                                fy.year = "2016-17",
                                sapto.eligible = TRUE,
                                new_sapto_tbl = copy(grattan:::sapto_tbl) %>% filter(fy_year == "2016-17") %>% setkeyv("family_status"),
                                medicare.sapto.eligible = TRUE), 
               income_tax(33000, "2016-17", age = 67))
})

test_that("New SAPTO matches old SAPTO for SAPTO", {
  expect_equal(new_sapto(33000, Spouse_income = 33000, family_status = "married",
                         new_sapto_tbl = copy(grattan:::sapto_tbl) %>% filter(fy_year == "2016-17") %>% setkeyv("family_status")), 
               sapto(33000, "2016-17", Spouse_income = 33000, family_status = "married"))
  expect_equal(income_tax_sapto(33000,
                                fy.year = "2016-17",
                                sapto.eligible = TRUE,
                                new_sapto_tbl = copy(grattan:::sapto_tbl) %>% filter(fy_year == "2016-17") %>% setkeyv("family_status"),
                                medicare.sapto.eligible = TRUE), 
               income_tax(33000, "2016-17", age = 67))
})

test_that("Works for ATO sample file", {
  skip_if_not_installed("taxstats")
  s1314 <- sample_file_1314 %>% copy
  tax_with_sapto_fn <-
    income_tax_sapto(s1314$Taxable_Income, fy.year = "2013-14",
                     .dots.ATO = copy(s1314),
                     age = if_else(s1314$age_range <= 1, 67, 42),
                     new_sapto_tbl = copy(grattan:::sapto_tbl) %>% filter(fy_year == "2013-14") %>% setkeyv("family_status"))
  tax_with_base_fn <- income_tax(s1314$Taxable_Income,
                                 fy.year = "2013-14",
                                 .dots.ATO = copy(s1314),
                                 age = if_else(s1314$age_range <= 1, 67, 42))
  
  expect_true(all(near(tax_with_base_fn, tax_with_sapto_fn)))
  
})


