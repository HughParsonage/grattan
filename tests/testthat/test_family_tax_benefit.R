context("Family tax Benefit")

test_that("Fringe cases", {
  library(data.table)
  #no income
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 5412.95, ftbB_incl_supplement = 4339.85))
  #single parent
  temp <- data.table(id_hh = c(1, 1), id = 1:2, age = c(35, 10), income = c(70000, 0),
                     in_secondary_school= FALSE, single_parent= TRUE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 2230.15, ftbB_incl_supplement = 3138.99),
               tol=1e-02)
  #18 yo child not in secondary school
  temp <- data.table(id_hh = c(1, 1), id = 1:2, age = c(35, 18), income = c(70000, 0),
                     in_secondary_school= FALSE, single_parent = c(TRUE, FALSE), other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 0, ftbB_incl_supplement = 0))
  #child receieving youth allowance
  temp <- data.table(id_hh = c(1, 1), id = 1:2, age = c(35, 10), income = c(70000, 0),
                     in_secondary_school= FALSE, single_parent= TRUE, other_allowance_benefit_or_pension = c(FALSE, TRUE) , maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 0, ftbB_incl_supplement = 0),
               tol=1e-02)
})

test_that("Benefit with 2 parent equal income", {
  library(data.table)
  #no income
  temp <- data.table(id_hh = c(1, 1, 1),
                     id = 1:3, age = c(35, 30, 5), income = c(0, 0, 0),
                     in_secondary_school= FALSE,
                     single_parent = FALSE,
                     other_allowance_benefit_or_pension = FALSE,
                     maintenance_income = 0,
                     maintenance_children = 0L)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 5412.95, ftbB_incl_supplement = 3139))
  expect_equal(family_tax_benefit(temp), 
               family_tax_benefit(id_hh = c(1, 1, 1),
                                  id = 1:3, age = c(35, 30, 5), income = c(0, 0, 0),
                                  in_secondary_school= FALSE,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L))
  
  
  #test1
  temp <- data.table(id_hh = c(1, 1, 1),
                     id = 1:3,
                     age = c(35, 30, 5),
                     income = c(30000, 30000, 0),
                     in_secondary_school= FALSE,
                     single_parent= FALSE, 
                     other_allowance_benefit_or_pension= FALSE,
                     maintenance_income = 0, 
                     maintenance_children = 0L)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 3618.35, ftbB_incl_supplement = 0))
  #base
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 5), income = c(45000, 45000, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 2230.15, ftbB_incl_supplement = 0))
  #test2
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 5), income = c(50000, 50000, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 524.95, ftbB_incl_supplement = 0))
  #max
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 5), income = c(51000, 51000, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = 0)
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 0, ftbB_incl_supplement = 0))
})

test_that("Maintenance", {
  library(data.table)
  #1 child
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 5), income = c(30000, 30000, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = c(3000, 0, 0), maintenance_children = c(1, 0, 0))
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement = 2889.95, ftbB_incl_supplement = 0),
               tol = 1e-02)
  #2 children same parent
  temp <- data.table(id_hh = c(1, 1, 1, 1), id = 1:4, age = c(35, 30, 5, 0), income = c(30000, 30000, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = c(6000, 0, 0, 0), maintenance_children = c(2, 0, 0, 0))
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement =  7060.6, ftbB_incl_supplement = 0),
               tol = 1e-02)
  #3 children different parents
  temp <- data.table(id_hh = c(1, 1, 1, 1, 1), id = 1:5, age = c(35, 30, 5, 0, 15), income = c(30000, 30000, 0, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = c(6000, 2000, 0, 0, 0), maintenance_children = c(2, 1, 0, 0, 0))
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = 1, ftbA_incl_supplement =  13658.08, ftbB_incl_supplement = 0),
               tol = 1e-02)
})

test_that("Multiple HH - combos of previous tests", {
  library(data.table)
  temp <- data.table(id_hh = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3), id = 1:10, age = c(35, 30, 5, 35, 10, 35, 30, 5, 0, 15), income = c(50000, 50000, 0, 70000, 0, 30000, 30000, 0, 0, 0),
                     in_secondary_school= FALSE, single_parent = c(rep(FALSE, 3), TRUE, rep(FALSE, 6)), other_allowance_benefit_or_pension= FALSE, 
                     maintenance_income = c(0, 0, 0, 0, 0, 6000, 2000, 0, 0, 0), maintenance_children = c(0, 0, 0, 0, 0, 2, 1, 0, 0, 0))
  expect_equal(family_tax_benefit(temp), 
               data.table(id_hh = c(1,2, 3), ftbA_incl_supplement = c(524.95, 2230.15, 13658.08), ftbB_incl_supplement = c(0, 3138.99, 0)),
               tol = 1e-02)
})

test_that("Errors", {
  library(data.table)
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = c(100, 0, 0), maintenance_children = 0)
  expect_error(family_tax_benefit(temp), 
               regexp = "Incompatible combination of `maintenance_income` and `maintenance_children`")
  temp <- data.table(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = c(1,0,0))
  expect_error(family_tax_benefit(temp), 
               regexp = "Incompatible combination of `maintenance_income` and `maintenance_children`")
  temp <- data.table(ERROR = c(1, 1, 1), id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                     in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = c(1,0,0))
  expect_error(family_tax_benefit(temp), 
               regexp = "`.data` did not contain a column .*id_hh")
  temp <- data.matrix(data.frame(id_hh = c(1, 1, 1), id = 1:3, age = c(35, 30, 0), income = c(0, 0, 0),
                                 in_secondary_school= FALSE, single_parent= FALSE, other_allowance_benefit_or_pension= FALSE, maintenance_income = 0, maintenance_children = c(1,0,0)))
  expect_error(family_tax_benefit(temp), 
               regexp = "`.data` is not of class `data.frame`.")
})

test_that("Errors during non .data arguments", {
  expect_error(family_tax_benefit(.data = NULL), 
               "`.data` was NULL, yet `id_hh` was also NULL.", 
               fixed = TRUE)
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = "50", 
                                  income = 50e3,
                                  in_secondary_school = 1L,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`age` was type.*numeric")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50,
                                  income = '50e3',
                                  in_secondary_school = 1L,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`income` was type.*numeric")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = 1L,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`in_secondary_school` was type.*logical")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = 1L,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`in_secondary_school` was type.*logical")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = FALSE,
                                  single_parent = 1,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`single_parent` was type.*logical")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = FALSE,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = 1,
                                  maintenance_income = 0,
                                  maintenance_children = 0L), 
               "`other_allowance_benefit_or_pension` was type.*logical")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = FALSE,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = '0',
                                  maintenance_children = 0L), 
               "`maintenance_income` was type.*numeric")
  expect_error(family_tax_benefit(id_hh = 1, id = 1, 
                                  age = 50, income = 50e3,
                                  in_secondary_school = FALSE,
                                  single_parent = FALSE,
                                  other_allowance_benefit_or_pension = FALSE,
                                  maintenance_income = 0,
                                  maintenance_children = '0'), 
               "`maintenance_children` was type.*integer")
})


