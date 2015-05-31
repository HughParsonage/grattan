


grsex <- function(value, digits = 1, format = "f", width = 2, smallest.units = "M", latex.big.mark = "\\,", abbrev = "singular") {
  if (grepl("^m", smallest.units, ignore.case = TRUE)){
    range <- as.numeric(cut(abs(value), breaks = c(0, 10^(3*(1:3)), Inf)))
    if (range == 1){
      value
    }
    
    if (range == 2){
      formatC(value, format = format, digits =  digits, width = width, big.mark = latex.big.mark)
    }
    
    if (range == 3){
      multiplier <- "million"
      if(grepl("^s", abbrev))
        paste(formatC(round(value/1e6, digits = digits), format = "f", width = 2, digits = digits), multiplier)
      if(grepl("^p", abbrev))
        paste(formatC(round(value/1e6, digits = digits), format = "f", width = 2, digits = digits), paste0(multiplier, "s"))
      if(grepl("^a", abbrev))
        paste(formatC(round(value/1e6, digits = digits), format = "f", width = 2, digits = digits), substr(multiplier, 0,))
      
    }
      
  }
  formatC(round(value, digits = digits), format = "f", width = 2, digits = digits)
}