#' attempt to harness ggplot but set as much to default as possible
#' 
#' 

gratplot_discrete <- function(...){
  ggplot2::ggplot(...) + 
    scale_color_discrete_grattan() + 
    scale_fill_discrete_grattan()
}

grplot <- function(..., theme.dots = ...){
  gratplot_discrete(...) + 
    theme_grattan()
}