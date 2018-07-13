context("Pension Supplement")

test_that("Rates", {
  #Max rates
  test_that(suppressWarnings(pension_supplement(has_partner = FALSE)),
            65)
  test_that(suppressWarnings(pension_supplement(has_partner = TRUE)),
            49)
  #basic rates
  test_that(suppressWarnings(pension_supplement(has_partner = FALSE, parenting_payment = TRUE)),
            22.70)
  test_that(suppressWarnings(pension_supplement(has_partner = FALSE, overseas_absence = TRUE)),
            22.70)
  test_that(suppressWarnings(pension_supplement(has_partner = TRUE, overseas_absence = TRUE)),
            18.7)
  })

test_that("Ineligibles", {
  test_that(suppressWarnings(pension_supplement(age_pension_age_requirement = TRUE, age = 20)),
            0)
  
  })