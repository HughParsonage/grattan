context("compare_avg_tax_rates")

test_that("Error handling", {
  dt1 <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                    baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                    new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.3, 
                    id = 1)
  dt1[, Taxable_Income := NULL]
  expect_error(compare_avg_tax_rates(dt1, dt1), 
               regexp = "Following names? not present in DT")
  dt1 <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                    baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                    new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.3, 
                    id = 1)
  dt2 <- copy(dt1)[, baseline_tax := NULL]
  expect_error(compare_avg_tax_rates(dt1, dt2), 
               regexp = "`baseDT` lacked a column `baseline_tax`.", 
               fixed = TRUE)
  expect_error(compare_avg_tax_rates(dt1, dt1[, WEIGHT := 1]), 
               regexp = "`DT` contained a column 'WEIGHT', yet for id = 1, sum(WEIGHT) = 4.", 
               fixed = TRUE)
  dt2 <- copy(dt1[, WEIGHT := NULL])
  expect_error(compare_avg_tax_rates(dt1[, WEIGHT := 10e6], dt2), 
               regexp = "`DT` contained a column 'WEIGHT', yet for id = 1, sum(WEIGHT) = 40,000,000.", 
               fixed = TRUE)
  expect_error(compare_avg_tax_rates(dt2, dt1[, WEIGHT := 10e6]), 
               regexp = "`baseDT` had a column called 'WEIGHT', yet sum(WEIGHT) was not between 10,000,000 and 20,000,000, likely a coding error.", 
               fixed = TRUE)
  
})

test_that("data.frame", {
  dt1 <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                    baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                    new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.3, 
                    id = 1)
  df1 <- as.data.frame(dt1)
  
  expect_identical(compare_avg_tax_rates(df1, df1), compare_avg_tax_rates(dt1, df1))
  expect_identical(compare_avg_tax_rates(dt1, dt1), compare_avg_tax_rates(df1, dt1))
})

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
  expect_equal(out2[["delta_avgTaxRate"]], rep(c(0.1, 0.2), each = 4))
  out2 <- compare_avg_tax_rates(rbind(newDT, newDT2), newDT, ids = c(1, 2))
  expect_equal(out2[["delta_avgTaxRate"]], rep(c(0.1, 0.2), each = 4))
  
})

test_that("ids that's not 'id'", {
  newDT <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                      baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                      new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.3, 
                      foo = 1)
  newDT2 <- data.table(Taxable_Income = c(50e3, 100e3, 150e3, 200e3), 
                       baseline_tax = c(50e3, 100e3, 150e3, 200e3) * 0.2,
                       new_tax = c(50e3, 100e3, 150e3, 200e3) * 0.4, 
                       foo = 2)
  
  out2 <- compare_avg_tax_rates(rbind(newDT, newDT2), 
                                baseDT = newDT, 
                                by = "foo",
                                ids = c(1, 2))
  expect_equal(out2[["delta_avgTaxRate"]], rep(c(0.1, 0.2), each = 4))
  
  
})

