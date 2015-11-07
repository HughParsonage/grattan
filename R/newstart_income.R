#' Newstart income
#' 
#' @param fortnightly.income income from other sources
#' @return the amount the Newstart recipient would receive in total
#' @note doesn't include Rent Assistance

newstart_income <- function(fortnightly.income){
  payment <- ifelse(fortnightly.income < 102,
                    523.4,
                    ifelse(fortnightly.income < 252,
                           523.4 - 0.5 * (fortnightly.income - 102),
                           pmax(0, 523.4 - 0.5 * (252 - 102) - 0.4 * (fortnightly.income - 252))))
  return(payment + fortnightly.income)
}