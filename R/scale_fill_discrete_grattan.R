#' 


scale_fill_discrete_grattan <- function(...) ggplot2::discrete_scale("fill", "hue", function(n) gpal(n), ...)