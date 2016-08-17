#' Newstart income
#' 
#' @param fortnightly.income income from other sources.
#' @return The amount the Newstart recipient would receive in total.
#' @note doesn't include Rent Assistance

newstart_income <- function(fortnightly.income){
  payment <- if_else(fortnightly.income < 102,
                     523.4,
                     if_else(fortnightly.income < 252,
                             523.4 - 0.5 * (fortnightly.income - 102),
                             pmaxC(523.4 - 0.5 * (252 - 102) - 0.4 * (fortnightly.income - 252), 0)))
  payment + fortnightly.income
}