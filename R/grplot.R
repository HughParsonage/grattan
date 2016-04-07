#' Attempt to harness ggplot but set as much to default as possible
#' 
#' @name grplot
#' @param ... arguments passed to ggplot
#' @export 

grplot <- function(...){
  ggplot2::update_geom_defaults("point", list(colour = grattan::Orange))  #but cf. col.3
  ggplot2::update_geom_defaults("bar", list(fill = grattan::DarkOrange, colour = "black"))
  ggplot2::update_geom_defaults("line", list(fill = grattan::Orange, colour = grattan::Orange, size = 2))
  ggplot2::ggplot(...) + 
    scale_color_discrete_grattan() + 
    scale_fill_discrete_grattan() + 
    theme_grattan()
}