#' Validate per
#' 
#' Checks whether a valid input of `per` is used and outputs the amount which yearly payments are divided by to get the desired rate. 
#' 
#' @param per How often are payments made? Can only take values 'year', 'fortnight', or 'quarter'.
#' @param missing_per Is `per`` missing in the outer function? If so the defualt for that function will be used.
#' @param .fortnights_per_yr What is the ratio of the fortnightly payment amount to the yearly payment amount? By default, 26. (Some payments expect 26; others expect 364/14.)
#' 
#' @examples 
#' See rent_assistance function

validate_per <- function(per, missing_per, .fortnights_per_yr = 26) {
  if (missing_per) {
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
    
    if (per == "annual") {
      per <- "year"
    } else if (per %notin% c("year", "fortnight", "quarter")) {
      stop("`per = '", per, "'` but must be one of 'year', 'fortnight', or 'quarter'. ")
    }
  }
  
  switch(per,
         fortnight = {
           if (is.numeric(.fortnights_per_yr)) {
             .fortnights_per_yr
           } else {
             stop("`per` was type '", typeof(per), "', but must be numeric.")
           }
         },
         quarter = 4,
         year = 1)
  
}
