#' Create waterfall charts
#' 
#' @name grattan waterfall
#' @param .data a placeholder for piped operations; cannot be used
#' @param values a numeric vector making up the heights of the rectangles in the waterfall
#' @param labels the labels corresponding to each vector, marked on the x-axis
#' @param rect_text_labels (character) a character vector of the same length as values that are placed on the rectangles 
#' @param rect_text_labels_anchor (character) How should rect_text_labels be positioned. In future releases, we might have support for north or south anchors, or for directed positioning (negative down, positive up) etc. For now, only centre is supported.
#' @param calc_total (logical) should the final pool of the waterfall be calculated (and placed on the chart)
#' @param total_axis_text (character) the text appearing on the axis underneath the total rectangle
#' @param total_rect_text (character) the text in the middle of the rectangle of the total rectangle
#' @param total_rect_color the color of the final rectangle
#' @param total_rect_text_color the color of the final rectangle's label text
#' @param fill_by_sign (logical) should positive and negative values each have the same colour?
#' @param dark (logical) (only for fill_by_sign) should the dark palette be used?
#' @param rect_width (numeric) the width of the rectangle, relative to the space between each label factor
#' @param rect_border the border around each rectangle. Choose NA if no border is desired.
#' @param draw_lines (logical) should lines be drawn between successive rectangles
#' @param linetype the linetype for the draw_lines
#' @param lines_anchors a character vector of length two specifying the horizontal placement of the drawn lines relative to the preceding and successive rectangles, respectively
#' @param draw_axis.x (character) one of "none", "behind", "front" whether to draw an x.axis line and whether to draw it behind or in front of the rectangles, default is behind
#' @param ggplot_object_name (character) a quoted valid object name to which ggplot layers may be addded after the function has run


grattan_waterfall <- function(.data = NULL,
                              values, labels, 
                              rect_text_labels = values,
                              rect_text_labels_anchor = "centre",
                              calc_total = FALSE,
                              total_axis_text = "Total",
                              total_rect_text = sum(values),
                              total_rect_color = gpal(6)[1],
                              total_rect_text_color = "white",
                              fill_colours = gpalx(length(values)),
                              fill_by_sign = TRUE,
                              dark = FALSE,
                              rect_width = 0.7,
                              rect_border = "black",
                              draw_lines = TRUE,
                              lines_anchors = c("centre", "centre"),
                              linetype = "dashed",
                              draw_axis.x = "behind",
                              ggplot_object_name = "mywaterfall"){
  if(!is.null(.data))
    warning(".data argument not yet supported")
  
  if(!(length(values) == length(labels) && length(labels) == length(fill_colours) && length(values) == length(rect_text_labels)))
    stop("values, labels, fill_colours, and rect_text_labels must all  have same length")
  
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
  
  if (!calc_total){
  p <- ggplot2::ggplot(data.frame(x = labels,
                                  y = values), aes(x = x, y = y)) + 
    ggplot2::geom_blank() + 
    grattan::theme_hugh() +
    ggplot2::theme(axis.title = element_blank())
  } else {
    p <- ggplot2::ggplot(data.frame(x = c(labels, total_axis_text),
                                    y = c(values, north_edge[number_of_rectangles])
                                    ), 
                                    aes(x = x, y = y)) + 
      ggplot2::geom_blank() + 
      grattan::theme_hugh() +
      ggplot2::theme(axis.title = element_blank())
  }
  
  if (grepl("behind", draw_axis.x)){
    p <- p + geom_hline(yintercept = 0)
  }
  
  for (i in seq_along(values)){
    p <- p + ggplot2::annotate("rect",
                               xmin = i - rect_width/2,
                               xmax = i + rect_width/2,
                               ymin = south_edge[i],
                               ymax = north_edge[i],
                               colour = rect_border,
                               fill = fill_colours[i])  
    if (i > 1 && draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = i - 1 - anchor_left,
                                 xend = i + anchor_right,
                                 linetype = linetype,
                                 y = south_edge[i],
                                 yend = south_edge[i])
    }
  }
  
  rect_text_labels
  
  for (i in seq_along(values)){
    p <- p + ggplot2::annotate("text",
                               x = i,
                               y = 0.5 * (north_edge[i] + south_edge[i]),
                               label = ifelse(rect_text_labels[i] == values[i],
                                              ifelse(values[i] < 0,
                                                     paste0("\U2212", -1 * values[i]),
                                                     values[i]),
                                              rect_text_labels[i]),
                               size = 7.14)
  }
  
  
  if (calc_total){
    p <- p + ggplot2::annotate("rect",
                               xmin = number_of_rectangles + 1 - rect_width/2,
                               xmax = number_of_rectangles + 1 + rect_width/2,
                               ymin = 0,
                               ymax = north_edge[number_of_rectangles],
                               colour = rect_border,
                               fill = total_rect_color)  +
      ggplot2::annotate("text",
                        x = number_of_rectangles + 1,
                        y = 0.5 * north_edge[number_of_rectangles],
                        label = ifelse(total_rect_text == sum(values),
                                       ifelse(north_edge[number_of_rectangles] < 0,
                                              paste0("\U2212", -1 * north_edge[number_of_rectangles]),
                                              north_edge[number_of_rectangles]),
                                       total_rect_text),
                        color = total_rect_text_color,
                        size = 7.14) + 
      scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines){
      p <- p + ggplot2::annotate("segment",
                        x = number_of_rectangles - anchor_left,
                        xend = number_of_rectangles + 1 + anchor_right,
                        y = north_edge[number_of_rectangles],
                        yend = north_edge[number_of_rectangles],
                        linetype = linetype) 
    }
  } else {
    p <- p + scale_x_discrete(labels = labels)
  }
  
  if (grepl("front", draw_axis.x)){
    p <- p + geom_hline(yintercept = 0)
  }
  
  print(p)
  # Allow modifications beyond the function call
  assign(ggplot_object_name, p, inherits = TRUE)
}