#' 
#' @param dt1 The baseline sample file, containing at least the variables \code{Taxable_Income}, \code{new_tax}, and \code{baseline_tax}.
#' @param ... Optionally, other sample files. 


compare_avg_tax_rates <- function(DT, baseDT, by = "id", ids = NULL) {
  required_cols <- c("Taxable_Income", by,
                     "new_tax", "baseline_tax")
  stopifnot(all(required_cols %chin% names(DT)))
  
  if (is.null(ids)) {
    out <- .selector(DT, required_cols)
  } else {
    out <- DT[id %ein% ids, .SD, .SDcols = c(required_cols)]
  }
  
  out[, Taxable_Income_percentile := weighted_ntile(Taxable_Income, n = 100), keyby = c(by)]
  
  baseline_avgTaxRate_by_Percentile <- 
    if ("baseline_tax" %in% names(baseDT) &&
        "Taxable_Income" %in% names(baseDT)) {
      # Much faster than DT[, .(x, y)]
      tax = .subset2(baseDT, "baseline_tax")
      Taxable_Income = .subset2(baseDT, "Taxable_Income")
      avgTaxRate = coalesce(tax / Taxable_Income, 0)
      
      setDT(list(baseline_tax = tax,
                 Taxable_Income = Taxable_Income,
                 avgTaxRate = avgTaxRate)) %>%
        .[, .(avgTaxRate = mean(avgTaxRate)),
          keyby = .(Taxable_Income_percentile = weighted_ntile(Taxable_Income, n = 100))]
    } else {
      stop("`baseDT` lacked a column `baseline_tax`.")
    }
  out[, new_avg_tax_rate := coalesce(new_tax / Taxable_Income, 0)]
  out[, .(new_avgTaxRate = mean(new_avg_tax_rate)), 
      keyby = c(by, "Taxable_Income_percentile")] %>%
    baseline_avgTaxRate_by_Percentile[, on = "Taxable_Income_percentile"] %>%
    .[, delta_avgTaxRate := new_avgTaxRate - avgTaxRate] %>%
    .[]
  
}



