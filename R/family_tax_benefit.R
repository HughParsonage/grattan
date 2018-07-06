#' FAMILY TAX BENEFIT
#' @param child_age
#' @param child_in_secondary_school
#' @param child_in_care
#' @export
#' 

family_tax_benefit <- function(child_age = 1,
                               child_in_secondary_school = FALSE,
                               child_in_care = FALSE){
  
  input <- data.table(do.call(cbind.data.frame, mget(ls())))
  
  max_rate_March_2016 <- 
    input[ ,if_else(child_age < 13,
                   179.76,
                   if_else(child_age <= 15,
                           233.94,
                           if_else(child_age <= 19 & child_in_secondary_school,
                                   233.94,
                                   if_else(child_in_care,
                                           57.68,
                                           0))))]
    
  base_rate_March_2016 <-
    57.68
  
  #SUPPLEMENT?
  
  
  #OUTPUT
  max_rate_March_2016
}
