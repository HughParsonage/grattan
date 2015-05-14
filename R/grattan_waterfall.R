#' Create waterfall charts
#' 
#' @name grattan waterfall
#' @param values a numeric vector making up the heights of the rectangles in the waterfall
#' @param labels the labels corresponding to each vector, marked on the x-axis
#' @param fill_by_sign (logical) should positive and negative values each have the same colour?
#' @param dark (logical) (only for fill_by_sign) should the dark palette be used?
#' @param rect_width (numeric) the width of the rectangle, relative to the space between each label factor
#' @param draw_lines (logical) should lines be drawn conneting successive rectangles
#' @param lines_anchors a character vector of length two specifying the horizontal placement of the drawn lines relative to the preceding and successive rectangles, respectively
#' @param ggplot_object_name (character) a quoted valid object name to which ggplot layers may be addded after the function has run
#' @value a ggplot2 object and the object name visibly to the parent environment


grattan_waterfall <- function(values, labels, 
                              fill_colours = gpalx(length(values)),
                              fill_by_sign = TRUE,
                              dark = FALSE,
                              rect_width = 0.7,
                              draw_lines = TRUE,
                              lines_anchors = c("centre", "centre"),
                              ggplot_object_name = "p"){
  if(!(length(values) == length(labels) && length(labels) == length(fill_colours)))
    stop("values, labels, and fill_colours must all  have same length")
  
  if(rect_width > 1)
    warning("rect_Width > 1, your chart may look terrible")
  
  number_of_rectangles <- length(values)
  north_edge <- cumsum(values)
  south_edge <- c(0,cumsum(values)[-length(values)])
  
  # fill by sign means rectangles' fill colour is given by whether they are going up or down
  if(fill_by_sign)
    fill_colours <- ifelse(values >= 0, gpal(2, dark=dark)[2], gpal(2, dark=dark)[1])
  
  if (!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", lines_anchors[2])))  # left right center
    stop("lines_anchors must be a pair of any of the following: left, right, centre")
  
  if (grepl("^l", lines_anchors[1])) 
    anchor_left <- rect_width / 2
  if (grepl("^c", lines_anchors[1]))
    anchor_left <- 0
  if (grepl("^r", lines_anchors[1]))
    anchor_left <- -1 * rect_width / 2
  
  if (grepl("^l", lines_anchors[2])) 
    anchor_right <- -1 * rect_width / 2
  if (grepl("^c", lines_anchors[2]))
    anchor_right <- 0
  if (grepl("^r", lines_anchors[2]))
    anchor_right <- rect_width / 2
  
  p <- ggplot2::ggplot(data.frame(x = labels,
                                  y = values), aes(x = x, y = y)) + 
    ggplot2::geom_blank() + 
    grattan::theme_hugh() 
  
  for (i in seq_along(values)){
    p <- p + ggplot2::annotate("rect",
                               xmin = i - rect_width/2,
                               xmax = i + rect_width/2,
                               ymin = south_edge[i],
                               ymax = north_edge[i],
                               fill = fill_colours[i])  +
      ggplot2::annotate("text",
                        x = i,
                        y = 0.5 * (north_edge[i] + south_edge[i]),
                        label = ifelse(values[i] < 0,
                                       paste0("\U2212", -1 * values[i]),
                                       values[i]),
                        size = 7.14)
    if (i > 1 && draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = i - 1 - anchor_left,
                                 xend = i + anchor_right,
                                 y = south_edge[i],
                                 yend = south_edge[i])
    }
  }
  print(p)
  # Allow modifications beyond the function call.
  "<<-"(ggplot_object_name, p)
}