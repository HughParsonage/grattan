

marginal_rate <- function(DT, fy.year, run = 100, 
                          col_taxable_income = "Taxable_Income") {
  Tax0 <- income_tax(.subset2(DT, col_taxable_income), fy.year = fy.year, .dots.ATO = DT)
  Tax1 <- income_tax(.subset2(DT, col_taxable_income) + run, fy.year = fy.year, .dots.ATO = DT)
  (Tax1 - Tax0) / run
}


