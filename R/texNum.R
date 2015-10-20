#' Convert number to English prose
#' 
#' @param sig.figs (not used)
#' 

texNum <- function(number, sig.figs = 3L){
  stopifnot(is.numeric(number))
  n.digits <- floor(log10(number))
  
  if (number == 0)
    return(0)
  
  suffix <- NULL
  out <- number
  
  rounded.number <- format(round(num, (1 + n.digits %% 3) - n.digits), scientific = FALSE)
  
  if (n.digits > 6){
    abbrev.number <- substr(number, 0, 1 + n.digits %% 3)
    suffix <- "million"
    
    if (n.digits > 8){
      suffix <- "billion"
    }
    
    if (n.digits > 12){
      suffix <- "trillion"
    }
    out <- paste0(abbrev.number, "~", suffix)
  }
  return(out)
}