#' Energy supplement
#' @description The ES is a supplementary payment that commenced on 20 September 2014. It was previously known as the clean energy supplement (CES). It is a fixed nominal amount; the supplement is neither indexed nor increased each year. There is no means testing.
#' @param qualifying_payment A string designating the payment type the individual is entitled to. Valid 
#' @param has_partner (logical, default: \code{FALSE}) Does the individual have a partner?
#' @param n_dependants How many dependants does the individual have? Default is zero.
#' @param age The age of the individual.
#' @param lives_at_home (logical, default: \code{FALSE}) Does the individual live at home?
#' @param independent (logical, default: \code{FALSE}) For persons under 21, is the person 'independent'?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?
#' @param long_term Is the individual a long-term welfare recipient?
#' @param per Dictates whether the result is per year or per fortnight.
#' 
#' @return The energy supplement for each individual. Arguments are recycled, but only if length-one.
#' @export energy_supplement
#' 

energy_supplement <- function(qualifying_payment, 
                              has_partner = FALSE,
                              n_dependants = 0L,
                              age = 21,
                              lives_at_home = FALSE,
                              independent = FALSE,
                              isjspceoalfofcoahodeoc = FALSE,
                              long_term = FALSE,
                              per = c("year", "fortnight")) {
  if (missing(qualifying_payment)) {
    stop("`qualifying_payment` is missing, with no default.")
  }
  
  max.length <-
    prohibit_vector_recycling.MAXLENGTH(qualifying_payment,
                                        has_partner,
                                        n_dependants,
                                        age, 
                                        lives_at_home, 
                                        independent,
                                        isjspceoalfofcoahodeoc,
                                        long_term)
  
  .qp <- tolower(qualifying_payment)
  if (length(.qp) != max.length) {
    .qp <- rep(.qp, times = max.length)
  }
  .qp[.qp == "age pension"] <- "pension"
  
  permitted <- c("pension", 
                 "seniors health card", 
                 "disability pension", 
                 "allowance", 
                 "parenting", 
                 "youth allowance", 
                 "austudy")
  
  if (!all(.qp %in% permitted)) {
    stop("`qualifying_payment` contains ", 
         qualifying_payment[qualifying_payment %notin% permitted])
  }
  
  rep_via <- function(v) {
    if (length(v) == 1L) {
      rep_len(v, max.length)
    } else {
      v
    }
  }
  
  qualifying_payment     %<>% rep_via
  has_partner            %<>% rep_via
  n_dependants           %<>% rep_via
  age                    %<>% rep_via
  lives_at_home          %<>% rep_via
  independent            %<>% rep_via
  isjspceoalfofcoahodeoc %<>% rep_via
  long_term              %<>% rep_via
  
  out <- 
    Switch(.qp, 
           "allowance" = {
             if_else(has_partner, 275.60, 366.60)
           }, 
           "seniors health card" = {
             if_else(has_partner, 275.60, 366.60)
           }, 
           "disability pension" = {
             if_else(has_partner | age > 20, 
                     236.60, 
                     if_else(age < 18, 
                             if_else(independent, 
                                     236.6, 
                                     153.40), 
                             if_else(independent, 
                                     236.6, 
                                     171.60)))
           }, 
           "allowance" = {
             if_else(has_partner,
                     205.4, 
                     if_else(isjspceoalfofcoahodeoc, 
                             312,
                             if_else(n_dependants > 0L | long_term, 
                                     247, 
                                     228.80)))
           }, 
           "parenting" = {
             if_else(has_partner, 205.40, 312)
           }, 
           "youth allowance" = {
             if_else(has_partner, 
                     if_else(n_dependants > 0L | long_term, 
                             200.20, 
                             182.00), 
                     if_else(isjspceoalfofcoahodeoc, 
                             312, 
                             if_else(n_dependants > 0L,
                                     239.20, 
                                     if_else(age < 18, 
                                             if_else(lives_at_home, 
                                                     101.40,
                                                     182),
                                             if_else(lives_at_home,
                                                     119.6,
                                                     182)))))
             
           },
           "austudy" = {
             if_else(has_partner,
                     if_else(n_dependants == 0L, 
                             182,
                             200.20),
                     if_else(n_dependants == 0L, 
                             182, 
                             239.20))
           })
  if (per[1L] == "fortnight") {
    out / 26
  } else {
    out
  }
  
}

