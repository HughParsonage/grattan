#' Validate per
#' 
#' @export validate_per
#' 

validate_per <- function(per = 'fortnight') {
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
  
}
