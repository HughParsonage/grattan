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


