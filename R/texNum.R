#' Convert number to English prose
#' 
#' @param sig.figs (not used)
#' 

texNum <- function(number, sig.figs = 3L, dollar = FALSE){
  stopifnot(is.numeric(number))
  is.negative <- number < 0
  number <- abs(number)
  if (number == 0){
    return(0)
  } else {
    n.digits <- ceiling(log10(number))
    
    suffix <- NULL
    out <- number
    
    if (n.digits < sig.figs){
      if (!is.negative){
        return(number)
      } else {
        return(paste0("$-$", number))
      }
    }
    
    
    if (n.digits <= 6){
      prefix <- prettyNum(round(number, sig.figs - n.digits - 1), big.mark = ",", scientific = FALSE)
    } 
    
    if (n.digits > 6){
      # Want to show only the number / 10^(multiple of 3) then the suffix multiplier
      prefix <- signif(number/10 ^ (3 * (n.digits %/% 3)), digits = sig.figs)
      suffix <- "~million"
      
      if (n.digits >= 9){
        suffix <- "~billion"
      }
      
      if (n.digits >= 12){
        suffix <- "~trillion"
      }
      
      if (n.digits > 15){
        prefix <- signif(number / 10^12, digits = sig.figs)
      }
      
    }
    
    if (dollar){
      out <- paste0("\\$", prefix, suffix)
    } else {
      out <- paste0(prefix, suffix)
    }
    
    if(is.negative){
      out <- paste0("$-$", out)
    }
    return(out)
  }
}
