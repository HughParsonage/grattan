test_that("2023-24 income tax applies the 45% top marginal rate", {
  income <- c(179999, 180000, 180001, 200000, 1000000)
  # ATO 2023-24 resident rates, including the 2% Medicare levy.
  # Above $180,000: $51,667 plus 45% of the excess, plus the levy.
  expect_equal(income_tax(income, "2023-24", age = 42),
               c(55266.61, 55267, 55267.47, 64667, 440667))
})
