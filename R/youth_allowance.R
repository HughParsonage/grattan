#' Youth allowance
#' 
#' @param eligible_if_over22 To be eligible for Youth Allowance while over 22, recipients must either commence full-time study or an Australian apprenticeship having been in receipt of an income support payment for at least 6 out of the last 9 months since turning 22, or study an approved course in English where English is not their first language.
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?



youth_allowance <- function(ordinary_income,
                            age = 18,
                            eligible_if_over22 = FALSE,
                            is_single = TRUE,
                            living_at_home = FALSE,
                            has_children = FALSE,
                            isjspceoalfofcoahodeoc = FALSE,
                            per = "fortnight") {
  stopifnot(per == "fortnight")
  
  max_rate_September_2017 <- 
    if_else(and(age >= 22,
                eligible_if_over22),
            if_else(is_single,
                    if_else(living_at_home,
                            353.50,
                            531.60),
                    480.50),
            # Under 22
            if_else(is_single,
                    if_else(and(age %between% c(16, 17), 
                                living_at_home),
                            239.50,
                            if_else(and(age >= 18, 
                                        living_at_home),
                                    288.10,
                                    if_else(and(age >= 16,
                                                !living_at_home),
                                            437.50,
                                            if_else(has_children,
                                                    573.30,
                                                    if_else(isjspceoalfofcoahodeoc,
                                                            752.60,
                                                            stop("Unknown recipient status for youth_allowance(). Check arguments.")))))),
                    # is partnered
                    if_else(has_children,
                            480.50,
                            437.50)))
  
}
