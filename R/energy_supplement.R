#' Energy supplement
#' @description The energy supplement (ES) is a supplementary payment that 
#' commenced on 20 September 2014. It was previously known as the clean energy 
#' supplement (CES). It is a fixed nominal amount; the supplement is neither 
#' indexed nor increased each year. There is no means testing.
#' @param qualifying_payment A character vector designating the payment type the individual is entitled to. Valid strings are
#' \describe{
#' \item{pension}{All pensions and bereavement allowance}
#' \item{seniors health card}{Commonwealth Seniors Health Card}
#' \item{disability pension}{Disability support pension (over 21)}
#' \item{allowance}{All allowances not elsewhere described, 
#' \emph{viz.} Newstart allowance, Widow allowance, Partner allowance, Sickness allowance}
#' \item{parenting}{Parenting payments}
#' \item{youth allowance}{Youth allowance (but not receiving youth disability supplement)}
#' \item{youth disability}{Youth allowance but also receiving youth disability supplement}
#' \item{austudy}{Austudy recipients}
#' }
#' @param has_partner (logical, default: \code{FALSE}) Does the individual have a partner? For persons with partners but separated due to the partner's illness or imprisonment, this may be true or false depending on the eligibility of the qualifying payment.
#' @param n_dependants How many dependants does the individual have? Default is zero.
#' @param age The age of the individual.
#' @param lives_at_home (logical, default: \code{FALSE}) Does the individual live at home?
#' @param independent (logical, default: \code{FALSE}) For persons under 21, is the person 'independent'?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?
#' @param long_term Is the individual a long-term welfare recipient?
#' @param per Dictates whether the result is per year, per fortnight, or per quarter. By default, yearly payments are returned, with a message. Payments are generally made each fortnight though recipients can elect to have them paid quarterly.
#' 
#' @return The energy supplement for each individual. Arguments are recycled, but only if length-one.
#' @export energy_supplement
#' @source 
#' \emph{Social Security Guide} by the Department of Social Services. 
#' Chapter 5, \sQuote{Payment rates}, s. 5.1.10.20 \dQuote{Clean Energy Household Assistance: current rates}.
#' \url{http://guides.dss.gov.au/guide-social-security-law/5/1/10/20}
#' 

energy_supplement <- function(qualifying_payment, 
                              has_partner = FALSE,
                              n_dependants = 0L,
                              age = 21,
                              lives_at_home = FALSE,
                              independent = FALSE,
                              isjspceoalfofcoahodeoc = FALSE,
                              long_term = FALSE,
                              per = c("year", "fortnight", "quarter")) {
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
  if (missing(per)) {
    per <- per[1L]
    message("`per` not set; calculating ", paste0(per, "ly"), " payment.")
  } else {
    if (!is.character(per)) {
      stop("`per` was type '", typeof(per), "', but must be a string.")
    }
    
    if (length(per) > 1L) {
      per <- per[1L]
      warning("`per` is provided but has length > 1 so only the first element ", 
              "(`per = '", per, "'`) will be used.")
    }
    
    if (per %notin% c("year", "fortnight", "quarter")) {
      stop("`per = '", per, "'` but must be one of 'year', 'fortnight', or 'quarter'. ")
    }
    
  }
  
  rep_via <- function(v) {
    if (length(v) == 1L) {
      rep_len(v, max.length)
    } else {
      v
    }
  }
  
  .qp <- rep_via(standardize_payment_names(qualifying_payment))
  .qp[.qp == "age pension"] <- "pension"
  
  permitted <- c("pension", 
                 "seniors health card", 
                 "disability pension", 
                 "allowance", 
                 "parenting", 
                 "youth allowance",
                 "youth disability",
                 "austudy")
  
  if (!all(.qp %in% permitted)) {
    stop("`qualifying_payment` contains '", 
         first(qualifying_payment[qualifying_payment %notin% permitted]),
         "'. ", 
         "The only valid entries are \n\t", 
         paste0(permitted, sep = "\n\t"), "\n",
         "Set other entries to an appropriate, valid payment.")
         
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
           "pension" = {
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
           
           "youth disability" = {
             if_else(has_partner, 
                     200.20,
                     if_else(lives_at_home, 
                             if_else(age < 18, 
                                     153.40,
                                     171.60),
                             221))
           },
           
           "austudy" = {
             if_else(has_partner,
                     if_else(n_dependants == 0L, 
                             182,
                             200.20),
                     if_else(n_dependants == 0L, 
                             182, 
                             239.20))
           },
           DEFAULT = 0)
  
  switch(per,
         "fortnight" = {
           out / 26
         },
         "year" = {
           out
         }, 
         "quarter" = {
           out / 4
         })
}

