context("SAPTO")


test_that("SAPTO for partners", {
  sapto(25000, fy.year = "2015-16", )
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


