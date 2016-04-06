#' Attempt to harness ggplot but set as much to default as possible
#' 
#' @name grplot
#' @param ... arguments passed to ggplot
#' @export 
#' 

grplot <- function(...){
  ggplot2::ggplot(...) + 
    scale_color_discrete_grattan() + 
    scale_fill_discrete_grattan() + 
    theme_grattan()
}