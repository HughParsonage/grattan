#' automatic palette adjustment (for small number of categories)
#' 
#' @name gpal
#' @param n the number of variables/factors over which the palette is to paint
#' @param dark Should a dark palette be used? (Only available for n=2)
#' @author Hugh Parsonage
#' @export
#' @return A vector of HTML colours to be used.

gpal <- function(n, dark = TRUE){
  stopifnot(requireNamespace("grattan", quietly = TRUE))
  grattan.palette <- list(pal.1, pal.2dark, pal.3, pal.4, pal.5, pal.6)
  if(n > 6){
    if(n > 9)
      stop('No palette available for that number of categories.', '\n', 'Consider using gpalx')
    else {
      grattan.palette <- list(pal.1, pal.2, pal.3, pal.4, pal.5, pal.6, pal.7, pal.8, pal.9)
      warning("I'm going off-piste: The Palette Of Nine is thine. May John have mercy on your soul.")
    }
  }
  if(!dark) {
    if (n == 2){
      pal.2
    } else {
      warning("no light option for palette ", n)
      grattan.palette[[n]]
    }             
  } else {
    grattan.palette[[n]]
  }
}
