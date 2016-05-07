#' automatic palette adjustment (for small number of categories)
#' 
#' @name gpal
#' @param n the number of variables/factors over which the palette is to paint
#' @param dark Should a dark palette be used? (Only available for n=2.)
#' @param reverse (logical) Option to reverse the palette.
#' @author Hugh Parsonage
#' @export
#' @return A vector of HTML colours to be used.

gpal <- function(n, dark = TRUE, reverse = FALSE){
  stopifnot(requireNamespace("grattan", quietly = TRUE))
  grattan.palette <- list(grattan::pal.1, grattan::pal.2dark, grattan::pal.3, grattan::pal.4, grattan::pal.5, grattan::pal.6)
  
  if(n > 6){
    if(n > 9)
      stop('No palette available for that number of categories.', '\n', 'Consider using gpalx')
    else {
      grattan.palette <- list(grattan::pal.1, grattan::pal.2, grattan::pal.3, grattan::pal.4, grattan::pal.5, grattan::pal.6, grattan::pal.7, grattan::pal.8, grattan::pal.9)
      warning("I'm going off-piste: The Palette Of Nine is thine. May John have mercy on your soul.")
    }
  }
  if(!dark) {
    if (n == 2){
      out <- grattan::pal.2
    } else {
      warning("no light option for palette ", n)
      out <- grattan.palette[[n]]
    }             
  } else {
    out <- grattan.palette[[n]]
  }
  if (reverse){
    rev(out)
  } else {
    out
  }
}
