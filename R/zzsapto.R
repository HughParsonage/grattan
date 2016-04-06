#' A function to calculate the Senior Australian and Pensioner Tax Offset
#' 
#' @param rebate_income The rebate income. See \code{?rebate_income}.
#' @param fy.year The financial year for which SAPTO is to be calculated. 
#' @param age (numeric) age in years
#' @param age_group (fctr) age in 5 year groups
#' @return the amount that will offset one's estimated tax


.sapto <- function(rebate_income, fy.year = "2012-13", age, age_group, family_status){
  if (fy.year != "2012-13"){
    stop("This function only works for fy.year = 2012-13")
  }
  if (missing(age) && missing(age_group))
    stop("At least one of 'age' and 'age_group' must be provided")
  
  if (!missing(age) && missing(age_group)){
    over65 <- age >= 65
  } 
  
  if (missing(age) && !missing(age_group)){
    over65 <- age_group >= "65 to 69"
  }
  
  if(!missing(age) && !missing(age_group)){
    over65 <- ifelse(xor(age >= 65, age_group >= "65 to 69"), NA, age >= 65)
  }
  
  is.single <- family_status == "individual"
  
  ifelse(!over65,
         0,
         ifelse(is.single,
                pmin(2230, pmaxC(2230 - (rebate_income - 32279) * 0.125, 0)),
                pmin(1602, pmaxC(1602 - (rebate_income - 28974) * 0.125, 0))
                )
         )
}
  
