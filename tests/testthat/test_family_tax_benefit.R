context("Family tax Benefit")

test_that("Benefit with no income", {
  #child aged 0
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                     in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 5412.95, ftbB_incl_supplement = 4339.85))
  #single parent
  temp <- data.table(HH_id = c(1, 1), pers_id = 1:2, age = c(35, 10), income = c(70000, 0),
                     in_secondary_school = F, single_parent = T, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 2230.15, ftbB_incl_supplement = 3138.99),
               tol=1e-02)
  #18 yo child not in secondary school
  temp <- data.table(HH_id = c(1, 1), pers_id = 1:2, age = c(35, 18), income = c(70000, 0),
                     in_secondary_school = F, single_parent = c(T, F), other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 0, ftbB_incl_supplement = 0))
})

test_that("Benefit with 2 parent equal income", {
  #no income
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 5), income = c(0, 0, 0),
                      in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
              data.table(HH_id = 1, ftbA_incl_supplement = 5412.95, ftbB_incl_supplement = 3139))
  #test1
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 5), income = c(30000, 30000, 0),
                     in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 3618.35, ftbB_incl_supplement = 0))
  #base
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 5), income = c(45000, 45000, 0),
                      in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 2230.15, ftbB_incl_supplement = 0))
  #test2
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 5), income = c(50000, 50000, 0),
                      in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 524.95, ftbB_incl_supplement = 0))
  #max
  temp <- data.table(HH_id = c(1, 1, 1), pers_id = 1:3, age = c(35, 30, 5), income = c(51000, 51000, 0),
                     in_secondary_school = F, single_parent = F, other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = 1, ftbA_incl_supplement = 0, ftbB_incl_supplement = 0))
})

test_that("Multiple HH - combos of previous tests", {
  temp <- data.table(HH_id = c(1, 1, 1, 2, 2), pers_id = 1:5, age = c(35, 30, 5, 35, 10), income = c(50000, 50000, 0, 70000, 0),
                     in_secondary_school = F, single_parent = c(F, F, F, T, F), other_allowance_benefit_or_pension = F)
  expect_equal(family_tax_benefit(temp), 
               data.table(HH_id = c(1,2), ftbA_incl_supplement = c(524.95, 2230.15), ftbB_incl_supplement = c(0, 3138.99)),
               tol = 1e-02)
})
