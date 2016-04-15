#' CGT inflator
#' 
#' For determing the size of capital gains tax in future years from previous values (i.e. from sample files)
#' 
#' @param cgt The amount to be inflated. 
#' @param from_fy The financial year of the current value of CGT.
#' @param to_fy The financial year for which CGT is intended to be inflated.
#' @return An estimate of the CGT payable in \code{to_fy}, inflated by relative tax expenditures. 
#' @importFrom magrittr %$%
#' @export CGT_inflator

CGT_inflator <- function(cgt = 1, from_fy, to_fy){
  stopifnot(length(from_fy) == 1L, length(to_fy) == 1L, from_fy %in% cgt_expenditures$FY, to_fy %in% cgt_expenditures$FY)
  rel_increase <- 
    cgt_expenditures$CGT_discount_for_individuals_and_trusts_millions[which(cgt_expenditures$FY == to_fy)] / 
    cgt_expenditures$CGT_discount_for_individuals_and_trusts_millions[which(cgt_expenditures$FY == from_fy)]
  
  cgt * rel_increase
}
