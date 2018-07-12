context("Pension Supplement")

test_that("", {
  test_that(pension_supplement(has_partner = FALSE),
            65)
  test_that(pension_supplement(has_partner = TRUE),
            95)
  
  
  })
