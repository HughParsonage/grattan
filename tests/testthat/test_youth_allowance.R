context("Youth allowance")

# http://guides.dss.gov.au/guide-social-security-law/5/5/2/40
test_that("Works with given DSS example", {
  out <- youth_allowance(ordinary_income = 180 + 10,
                         age = 21,
                         is_single = TRUE, 
                         living_at_home = FALSE,
                         has_children = FALSE, 
                         per = "fortnight")
  expect_equal(out, 437.50)
})