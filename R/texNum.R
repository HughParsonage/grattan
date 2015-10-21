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
  
  if (n.digits < sig.figs)
    return(number)
  
  
  if(n.digits <= 6){
    prefix <- prettyNum(round(number, sig.figs - n.digits - 1))
  } else
    prefix <- format(round(number, (1 + n.digits %% 3) - n.digits), scientific = FALSE)
  
  
  if (n.digits > 6){
    # prefix <- substr(number, 0, 1 + n.digits %% 3)
    prefix <- substr(number, 0, sig.figs)
    suffix <- "million"
    
    if (n.digits > 8){
      suffix <- "billion"
    }
    
    if (n.digits > 12){
      suffix <- "trillion"
    }
    
  }
  out <- paste0(prefix, "~", suffix)
  return(out)
}