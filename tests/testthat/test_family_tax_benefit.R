context("Family tax Benefit")

test_that("Benefit with no income", {
  expect_equal(family_tax_benefit(data.table(HH_id = c(1, 1),pers_id = 1:2, age = c(40, 0), income = c(0,0))),
               179.76)
  expect_equal(family_tax_benefit(data.table(HH_id = c(1, 1),pers_id = 1:2, age = c(40, 15), income = c(0,0))),
               233.94)
  expect_equal(family_tax_benefit(data.table(HH_id = c(1, 1),pers_id = 1:2, age = c(40, 18), income = c(0,0), in_secondary_school = c(FALSE,TRUE))),
               233.94)
  expect_equal(family_tax_benefit(data.table(HH_id = c(1, 1),pers_id = 1:2, age = c(40, 4), income = c(0,0), in_care = c(FALSE,TRUE))),
               57.68)
})