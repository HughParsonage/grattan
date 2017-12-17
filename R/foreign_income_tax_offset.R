#' Foreign Income Tax Offset
#' @param tax_payable The income tax payable (including Medicare levy, Medicare levy surcharge and temporary budget repair levy) for the relevant income year, excluding penalties and interest and disregarding any tax offsets.
#' @param foreign_assessable_income The amount of taxable income on which foreign income has been paid.
#' @param other_foreign_income Other income or gains from a non-Australian source.
#' @param foreign_entitlements The sum of 
#' \itemize{
#' \item{debt deductions attributable to your overseas permanent establishment}
#' \item{any other deductions (other than debt deductions) that are reasonably related to any amount covered by the first dot point above}
#' \item{an amount of the foreign loss component of one or more tax losses deducted in the income year.}
#' }
#' @export 

foreign_income_tax_offset <- function(tax_payable,
                                      fy_year,
                                      foreign_assessable_income, 
                                      other_foreign_income, 
                                      foreign_entitlements = 0) {
  offset_limit <- tax_payable
}
