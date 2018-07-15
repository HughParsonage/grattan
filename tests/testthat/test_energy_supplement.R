context("Energy supplement")

test_that("Error handling", {
  expect_error(energy_supplement(), 
               regexp = "`qualifying_payment` is missing, with no default.", 
               fixed = TRUE)
  expect_error(energy_supplement("blooz"), 
               regexp = "`qualifying_payment` contains 'blooz'", 
               fixed = TRUE)
})

test_that("Correct", {
  # http://guides.dss.gov.au/guide-social-security-law/5/1/10/20
  ee <- expect_equal
  f <- energy_supplement
  
  ee(f("age pension", has_partner = FALSE), 366.60)
  ee(f("age pension", has_partner = TRUE), 275.60)
  ee(f("seniors health card", has_partner = FALSE), 366.60)
  ee(f("seniors health card", has_partner = TRUE), 275.60)
  ee(f("disability pension", has_partner = FALSE, age = 17, independent = FALSE), 153.40)
  ee(f("disability pension", has_partner = FALSE, age = 17, independent = TRUE), 236.60)
  ee(f("disability pension", has_partner = FALSE, age = 18, independent = FALSE), 171.60)
  ee(f("disability pension", has_partner = FALSE, age = 18, independent = TRUE), 236.60)
  ee(f("disability pension", has_partner = TRUE), 236.60)
  ee(f("allowance", has_partner = FALSE, n_dependants = 0L), 228.80)
  ee(f("allowance", has_partner = FALSE, n_dependants = 1L), 247.00)
  ee(f("allowance", has_partner = FALSE, age = 60, long_term = TRUE), 247.00)
  ee(f("allowance", has_partner = TRUE), 205.40)
  ee(f("allowance", has_partner = FALSE, isjspceoalfofcoahodeoc = TRUE), 312.00)
  ee(f("parenting", has_partner = FALSE), 312.00)
  ee(f("youth allowance", has_partner = FALSE, age = 17, lives_at_home = TRUE), 101.40)
  ee(f("youth allowance", has_partner = FALSE, age = 17, lives_at_home = FALSE), 182.00)
  ee(f("youth allowance", has_partner = FALSE, age = 18, lives_at_home = TRUE), 119.60)
  ee(f("youth allowance", has_partner = FALSE, age = 18, lives_at_home = FALSE), 182.00)
  ee(f("youth allowance", has_partner = FALSE, n_dependants = 1L), 239.20)
  ee(f("youth allowance", has_partner = TRUE, n_dependants = 0L), 182)
  ee(f("youth allowance", has_partner = TRUE, n_dependants = 1L), 200.20)
  ee(f("youth allowance", has_partner = FALSE, n_dependants = 1L, isjspceoalfofcoahodeoc = TRUE), 312.00)
  ## ...
  ee(f("austudy", has_partner = FALSE, n_dependants = 0L, isjspceoalfofcoahodeoc = FALSE), 182.00)
  ee(f("austudy", has_partner = FALSE, n_dependants = 1L, isjspceoalfofcoahodeoc = FALSE), 239.20)
  ee(f("austudy", has_partner = TRUE, n_dependants = 0L, isjspceoalfofcoahodeoc = FALSE), 182.00)
  ee(f("austudy", has_partner = TRUE, n_dependants = 1L, isjspceoalfofcoahodeoc = FALSE), 200.20)
  ee(f(c("austudy", "age pension"), has_partner = TRUE, n_dependants = 1L, isjspceoalfofcoahodeoc = FALSE),
     c(200.20, 275.60))
})

test_that("per", {
  expect_equal(energy_supplement("austudy",
                                 has_partner = TRUE,
                                 n_dependants = c(0L, 1L, 0L), 
                                 per = "fortnight"), 
               c(182, 200.2, 182) / 26)
})

test_that("Multi-length", {
  expect_equal(energy_supplement("austudy",
                                 has_partner = TRUE,
                                 n_dependants = c(0L, 1L, 0L)), 
               c(182, 200.2, 182))
})
