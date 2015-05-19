#' for automatic use of the palette
#' 

scale_color_discrete_grattan <-  function(...) ggplot2::discrete_scale("colour", "hue", function(n) gpal(n), ...)
scale_colour_discrete_grattan <- function(...) scale_color_discrete_grattan(...)
