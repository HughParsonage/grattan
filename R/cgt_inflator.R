#' CGT inflator
#' 
#' For determing the size of capital gains tax in future years from previous values (i.e. from sample files)
#' 
#' @param cgt The amount to be inflated. 
#' @param from_fy The financial year of the current value of CGT.
#' @param to_fy The financial year for which CGT is intended to be inflated.
#' @return An estimate of the CGT payable in \code{to_fy}, inflated by relative tax expenditures. 

CGT_inflator <- function(cgt = 1, from_fy, to_fy){
  grattan:::.cgt_expenditures %T>%
  {
    stopifnot(length(from_fy) == 1L, length(to_fy) == 1L, from_fy %in% .$FY, to_fy %in% .$FY) 
  } %>%
  {
    .$CGT_discount_for_individuals_and_trusts_millions[which(.$FY == to_fy)] / 
      .$CGT_discount_for_individuals_and_trusts_millions[which(.$FY == from_fy)]
  }
}
