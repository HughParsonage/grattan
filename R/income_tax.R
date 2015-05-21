#' tax function
#' 
#' @param income the personal assessable income
#' @param ML.lower the lower medicare threshold
#' @param ML.upper the upper medicare threshold
#' @export 
#' @author Various
#' @return the total personal income tax payable

income_tax <- function(income, ML.lower, ML.upper, fy.year = "2013-14"){
  tax <- ifelse(income < 18200, 
                0, 
                ifelse(income < 37000, 
                       0.19*(income - 18200), 
                       ifelse(income < 80000, 
                              3572 + 0.325*(income - 37000),
                              ifelse(income < 180000, 
                                     17547 + 0.39*(income - 80000), 
                                     54547 + 0.45*(income - 180000)))))
  medicare.levy <- ifelse(income < ML.lower, 
                          0, 
                          ifelse(income < ML.upper, 
                                 0.10*(income - ML.lower), 
                                 0.02*income))
  
  tax + medicare.levy
}