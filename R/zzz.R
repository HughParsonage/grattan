.onLoad <- function(libname = find.package("grattan"), pkgname = "grattan"){
  ggplot2::update_geom_defaults("point", list(colour = Orange))  #but cf. col.3
  ggplot2::update_geom_defaults("bar", list(fill = DarkOrange, colour = "black", width = 0.7))
  ggplot2::update_geom_defaults("line", list(fill = Orange, colour = Orange, size = 2))
}
