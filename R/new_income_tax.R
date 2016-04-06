#' New income tax payable
#' Income tax payable with new tax brackets, tax rates etc
#' 
#' @param income A vector of taxable incomes.
#' @param sapto_rebate_income A vector of the rebate income (for SAPTO). By default, this is equal to income. See \code{?rebate_income}.
#' @param age A numeric vector of ages, in years. 
#' @param brackets A numeric vector giving the (lower boundaries of the) tax brackets that determine the base rate of tax paid. The first entry should be the tax-free threshold; that is, do not start with 0.
#' @param marginal_rates A numeric vector giving the marginal rates of tax to determine the base rate of tax paid. The rates correspond to the brackets above the tax-free threshold. \code{marginal_rates[i]} is the marginal rate of tax for the bracket \code{[brackets[i], brackets[i + 1]}. In particular, do not include zero as the first entry.
#' @param medicare.lower A single value providing the threshold of \code{income} at and below which no medicare levy is payable.
#' @param medicare.upper A single value providing the upper threshold of \code{income} below which the medicare levy reduction for low-income earners applies, and above which the full medicare levy applies. If you do not wish to have a medicare levy reduction, set this to be \code{medicare.lower} but be sure to set \code{medicare.taper} to a fixed number.
#' @param medicare.rate The rate at which the medicare levy is applied to taxable income. This was 2\% in 2015-16.
#' @param medicare.taper The rate at which the medicare levy increases for every dollar of taxable income above \code{medicare.lower}. By default it is equal to the implicit taper rate -- that is, the rate determined by the \code{medicare.lower} and \code{medicare.upper} parameters and the \code{medicare.rate}.
#' @param max_lito A single value, representing the maximum low-income tax offset available. 
#' @param lito_taper The rate at which the tax offsets reduces with every dollar increase in taxable income.
#' @param lito_min_bracket A single value, representing the taxable income at which the lito_taper starts to apply.
#' @param sapto_max_offset The maximum value of the seniors and pensioners offset.
#' @param sapto_lower_threshold The maximum taxable income for which \code{sapto_max_offset} is available
#' @param sapto_upper_threshold The value above which no SAPTO is available.
#' @param sapto_taper_rate The rate at which the sapto reduces for every dollar of \code{income} above \code{sapto_lower_threshold}. By default, it is determined by \code{sapto_lower_threshold} and \code{sapto_upper_threshold}.

new_income_tax <- function(income, 
                           sapto_rebate_income = income,
                           age = 42, 
                           brackets, marginal_rates, 
                           medicare.lower = 20542, 
                           medicare.upper = 24167, 
                           medicare.rate = 0.02, 
                           medicare.taper = (medicare.upper * medicare.rate)/(medicare.upper - medicare.lower),
                           max_lito = 445,
                           lito_taper = 0.015,
                           lito_min_bracket = 37000,
                           sapto_max_offset = 2230,
                           sapto_lower_threshold = 32279, 
                           sapto_upper_threshold = 50119,
                           sapto_taper_rate = sapto_max_offset/(sapto_upper_threshold - sapto_lower_threshold)){
  # CRAN
  marginal_rate <- NULL; lower_bracket <- NULL; tax_at <- NULL; tax <- NULL
  
  # Assume SAPTO eligibility is only a matter of age and income
  # and that every man is an island.
  ordering <- rank(income, ties.method = "first")
  
  tax_table2 <- 
    data.table::data.table(
      # brackets always start from zero
      lower_bracket = c(0, brackets),
      marginal_rate = c(0, marginal_rates)
    ) %>% 
    dplyr::mutate(
      tax_at = cumsum(lag(marginal_rate, default = 0) * (lower_bracket - lag(lower_bracket, default = 0)))
    ) %>%
    dplyr::mutate(income = lower_bracket) %>%
    data.table::setkey(income) %>%
    dplyr::select(income, lower_bracket, marginal_rate, tax_at)
  
  input <- 
    data.table::data.table(ordering = 1:length(income),
                           income = income, 
                           age = age) %>%
    setkey(income)
  
  base_tax.tbl <- 
    tax_table2[input, roll = Inf][,tax := tax_at + (income - lower_bracket) * marginal_rate]
  base_tax <- base_tax.tbl$tax[ordering]
  
  ## Medicare levy
  medicare_levy <- 
    pmaxC(pminV(medicare.taper * (income - medicare.lower), 
                medicare.rate * income),
          0)
  
  lito <- 
    pmaxC(max_lito - (income - lito_min_bracket) * lito_taper, 
          0)
  
  sapto <- 
    pmaxC(pminV(sapto_max_offset, 
                sapto_upper_threshold * sapto_taper_rate - sapto_rebate_income * sapto_taper_rate),
          0)
  
  pmaxC(base_tax + medicare_levy - lito - (age > 65) * sapto,
        0)
}