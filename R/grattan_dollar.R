#' dollar scales
#' 
#' @details makes negative ten Gs appears as -\$10,000 instead of \$-10,000

# from scales
grattan_dollar <- function (x, digits = 0) 
{
  #
  nsmall <- digits
  commaz <- format(abs(x), nsmall = nsmall, trim = TRUE, big.mark = ",", 
                   scientific = FALSE, digits = 1L)
  
  ifelse(x < 0, 
         paste0("\U2212","$", commaz),
         paste0("$", commaz))
}
