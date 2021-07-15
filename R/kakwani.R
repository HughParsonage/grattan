


kakwani2 <- function(income, tax) {
  tot_income <- sum(income)
  tot_tax <- sum(tax)
  
  alpha <- (income / tot_income)
  gamma <- (tax / tot_tax)
  
  inner_sum <- cumsum(alpha - gamma)
  sum(inner_sum) * 2 / length(income)
}

