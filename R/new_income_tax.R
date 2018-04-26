#' New income tax payable
#' Income tax payable with new tax brackets, tax rates etc
#' 
#' @param income A vector of taxable incomes.
#' @param new_tax_tbl A \code{data.table} with columns \code{lower_bracket} and \code{marginal_rate} for the new brackets and marginal rates.
#' @return The income according to the new parameters.
#' @export 

new_income_tax <- function(income, 
                           new_tax_tbl){
  stopifnot(is.data.table(new_tax_tbl))
  stopifnot(all(c("lower_bracket", "marginal_rate") %in% names(new_tax_tbl)))
  
  # CRAN avoidance 
  marginal_rate <- 
    lower_bracket <- 
    tax_at <- 
    ordering <- 
    tax <- NULL
  
  tax_table2 <- 
    new_tax_tbl %>%
    copy %>%
    .[, tax_at := cumsum(shift(marginal_rate, type = "lag", fill = 0) * (lower_bracket - shift(lower_bracket, type = "lag", fill = 0)))] %>%
    .[, income := lower_bracket] %>%
    .[, list(income, lower_bracket, marginal_rate, tax_at)] %>%
    setkeyv("income") 
  
  input <- 
    data.table(income = income) 
  
  input <- input[, ordering := 1:.N]
  
  input.keyed <-
    # potentially expensive. Another way would be 
    # to add an argument such as data.table.copy = FALSE
    # to allow different way to preserve the order
    copy(input) %>%
    setkeyv("income")
  
  tax_fun <- function(income){
    tax_table2[input.keyed, roll = Inf] %>%
      .[, tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
      .[order(ordering)] %>%
      .[["tax"]]
  }
  
  tax_fun(income)
}

