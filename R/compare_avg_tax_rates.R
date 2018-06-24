#' Compare average tax rates by percentile
#' @description To determine the effects of bracket creep on a proposed tax policy,
#' a common task is calculate the change in the average tax rates for each percentile.
#' This function accepts a sample file and a baseline sample file, and returns a 100-row table
#' giving the mean change in average tax rates for each percentile, compared to the baseline.
#' @param DT A single \code{data.table} containing columns \code{new_tax},
#' \code{Taxable_Income}, \code{baseline_tax}.
#' @param baseDT A \code{data.table} of a single cross-section of taxpayers from
#' which baseline percentiles can be produced. 
#' @param by How to separate \code{DT} 
#' @param ids Subset \code{DT} by \code{by}.
#' @export


compare_avg_tax_rates <- function(DT, baseDT, by = "id", ids = NULL) {
  if (!is.data.table(DT)) {
    DT <- as.data.table(DT)
  }
  if (!is.data.table(baseDT)) {
    baseDT <- as.data.table(baseDT)
  }
  
  required_cols <- c("Taxable_Income", by,
                     "new_tax", "baseline_tax")
  if (!all(required_cols %chin% names(DT))) {
    stop("Following names not present in DT:\n", 
         paste0(setdiff(required_cols, names(DT)), collapse = "\t\n"))
  }
  
  if (is.null(ids)) {
    out <- .selector(DT, required_cols)
  } else {
    if (identical(by, "id")) {
      id <- NULL
      out <- .selector(DT[id %ein% ids], required_cols)
    } else {
      out <- .selector(DT[eval(parse(text = by)) %ein% ids], required_cols)
    }
  }
  
  if ("WEIGHT" %chin% names(DT)) {
    pop <- WEIGHT <- NULL
    populations <- DT[, .(pop = sum(WEIGHT)), keyby = c(by)]
    min_pop <- populations[, min(pop)]
    max_pop <- populations[, max(pop)]
    if (min_pop < 10e6) {
      min_by <- populations[pop == min_pop]
      stop("`DT` contained a column 'WEIGHT', yet for ", 
           "", by, " = ", min_by[[by]],
           ", sum(WEIGHT) = ", format(min_pop, big.mark = ",", scientific = FALSE), ". ", 
           "Since this is less than 10 000 000, it is assumed to be a coding error. ", 
           "(Did you use the wrong column '", by, "'?)")
    }
    if (max_pop > 20e6) {
      max_by <- populations[pop == max_pop]
      stop("`DT` contained a column 'WEIGHT', yet for ", 
           "", by, " = ", max_by[[by]],
           ", sum(WEIGHT) = ", format(max_pop, big.mark = ",", scientific = FALSE), ". ", 
           "Since this is greater than 20 000 000, it is assumed to be a coding error. ", 
           "(Did you use the wrong column '", by, "'?)")
    }
  }
  
  if ("WEIGHT" %chin% names(baseDT)) {
    WEIGHT <- NULL
    base_population <- baseDT[, sum(WEIGHT)]
    if (base_population < 10e6 || base_population > 20e6) {
      stop("`baseDT` had a column called 'WEIGHT', yet sum(WEIGHT) was ",
           "not between 10,000,000 and 20,000,000, likely a coding error.")
    }
  }
  
  Taxable_Income <- Taxable_Income_percentile <- NULL
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
  new_avgTaxRate <- avgTaxRate <- new_avg_tax_rate <- new_tax <- NULL
  out[, new_avg_tax_rate := coalesce(new_tax / Taxable_Income, 0)]
  out[, .(new_avgTaxRate = mean(new_avg_tax_rate)), 
      keyby = c(by, "Taxable_Income_percentile")] %>%
    baseline_avgTaxRate_by_Percentile[., on = "Taxable_Income_percentile"] %>%
    .[, 'delta_avgTaxRate' := new_avgTaxRate - avgTaxRate] %>%
    .[]
  
}






