#' scale fill scale for grattan (uses the palette)
#' @export
#' @param ... arguments passed to discrete_scale.
#' @param reverse (logical) Option to reverse the palette.
#' @details A function controlling the colors.

scale_fill_discrete_grattan <- function(..., reverse = FALSE) ggplot2::discrete_scale("fill", "hue", function(n) gpal(n, reverse = reverse), ...)