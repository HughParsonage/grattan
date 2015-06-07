#' A theme for use with ggplots
#' 
#' @name theme_huh
#' 

theme_hugh <- function(base_size = 22, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.5, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#000000", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9),
      
      # Modified inheritance structure of text element
      #       plot.title =        element_text(size = rel(1.0), family = '' , 
      #                                        face = '', hjust = -0.05, 
      #                                        vjust = 1.5, colour = '#3B3B3B'),
      plot.title = element_text(hjust = 0, vjust = 1.5),
      # Puts x lab at the far right
      axis.title.x =      element_text(hjust = 1, vjust = -0.33),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      axis.line =         element_line(color = "black"),
      axis.ticks.length = grid::unit(0.3, "lines"),
      axis.ticks.margin = grid::unit(0.5, "lines"), 
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(color = "black"),
      panel.grid.major =  element_line(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_blank(),
      plot.margin = grid::unit(c(0.1,0.1,0.1,0.1), "lines"),
      panel.background =  element_blank(),
      legend.key =        element_rect(fill='white'),
      
      # Modifiying legend.position
      legend.position = 'none',
      legend.background = element_rect(fill = NA, colour = NA),
      strip.background = element_rect(fill = "white"),
      
      complete = TRUE
    )
}