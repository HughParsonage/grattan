context("compare_avg_tax_rates")

test_that("Expected output", {
  library(data.table)
  newDT <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                      baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                      new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.3, 
                      id = 1)
  out1 <- compare_avg_tax_rates(newDT, newDT)
  expect_equal(out1[["delta_avgTaxRate"]], rep(0.1, 4))
  
  
  newDT2 <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                       baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                       new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.4, 
                       id = 2)
  out2 <- compare_avg_tax_rates(rbind(newDT, newDT2), newDT)
  expect_equal(out2[["delta_avgTaxRate"]], rep(c(0.1, 0.2), 4))
  out2 <- compare_avg_tax_rates(rbind(newDT, newDT2), newDT, ids = c(1, 2))
  expect_equal(out2[["delta_avgTaxRate"]], rep(c(0.1, 0.2), 4))
})

