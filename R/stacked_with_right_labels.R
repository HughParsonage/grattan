#' Stacked charts with labels at right
#' 
#' @param .data A data frame, containing entries for \code{x}, \code{y}, and \code{fill}. \code{x} and \code{fill} must be ordered factors.
#' @param geom The type of chart ("bar", "area").
#' @param barwidth Passed to the \code{width} argument of \code{geom_bar}
#' @param verbose Report the margin used (in grid:: 'lines').
#' @param right_margin The amount of padding at right to use. The whole point of this function is to select a good right margin to allow space. But if the margin provided is wrong, it can be changed manually here.
#' @param scale_y_args A list of arguments passed to r \code{ggplot2::scale_y_continuous}.
#' @param x_continuous Should the x axis be continuous?
#' @param scale_x_args A list of arguments passed to \code{ggplot2::scale_x_discrete}. If \code{x_continuous}, then the arguments passed to \code{ggplot2::scale_x_continuous}.
#' @param coord_cartesian_args A list of arguments passed to \code{ggplot2::coord_cartesian}.
#' @param text_family Text family for theme and geom text. 
#' @param annotate_args Arguments passed to \code{ggplot2::annotate}.
#' @param theme_grattan.args Arguments passed to \code{theme_hugh}, an alias for \code{theme_grattan}. (For example, the \code{base_size}.)
#' @param theme.args A list of arguments passed to \code{ggplot2::theme}.
#' @param nudge_up A numeric vector to be added every text y-coordinate.
#' @param nudge_right Move text right in units of \code{x}.
#' @param extra_left_spaces Number of space characters \code{" "} preceding the text labels. Extra space characters are added before every newline.
#' @return A chart with the labels in the right gutter 
#' @importFrom graphics strwidth
#' @examples 
#' library(data.table)
#' dat <- data.table::CJ(
#'   x = factor(1:10, ordered = TRUE),
#'   fill = factor(c("A long but not\ntoo long label", letters[2:3]),
#'                 levels = c("A long but not\ntoo long label", letters[2:3]),
#'                 ordered = TRUE)
#' ) 
#' dat$y <- abs(rnorm(1:nrow(dat)))
#' 
#' stacked_bar_with_right_labels(dat)
#' 
#' 
#' @export



stacked_bar_with_right_labels <- function(.data, 
                                          geom = "bar",
                                          barwidth,
                                          verbose = FALSE,
                                          right_margin = 0.5,
                                          scale_y_args,
                                          x_continuous = FALSE,
                                          scale_x_args,
                                          coord_cartesian_args,
                                          text_family = "",
                                          annotate_args,
                                          theme_grattan.args,
                                          theme.args, 
                                          nudge_up = 0, 
                                          nudge_right = 0.5, 
                                          extra_left_spaces = 0L){
  stopifnot(all(c("x", "y", "fill") %in% names(.data)))
  x = y = fill = text.label = text.x = text.y = NULL
  if(!is.factor(.data$fill) || !is.ordered(.data$fill)){
    stop("'fill' must be an ordered factor.")
  }
  if (!x_continuous){
    if (!is.factor(.data$x) || !is.ordered(.data$x)){
    stop("'x' must be an ordered factor.")
    }
  } else {
    if (!is.numeric(.data$x)){
      stop("x must be numeric")
    }
  }
  
  .plot.data <- 
    .data %>%
    # our label should only appear at the last x
    dplyr::mutate(text.label = if_else(x == max(x), 
                                       paste0(paste0(rep(" ", extra_left_spaces), collapse = ""), 
                                              gsub("\n", 
                                                   # Add extra white space (push to right margin)
                                                   paste0(rep(" ", extra_left_spaces), collapse = ""), 
                                                   as.character(fill), 
                                                   fixed = TRUE)),
                                       NA_character_)) %>%
    # it should be as high as the corresponding bar:
    # all the way up the previous, then half of the corresponding height
    dplyr::arrange(fill) %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(text.y = -y/2 + cumsum(y) + nudge_up) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(text.x = max(as.numeric(.data$x)) + nudge_right)
  
  
  label_max_width <- 
    # longest spell between '\n <---> \n'
    strsplit(as.character(unique(.data$fill)), split = "\n") %>%
    unlist %>%
    # actual character size in bold `Arial'
    strwidth(., units = "inches", font = 2, family = "sans") %>%
    max
  
  # To convert to lines, use "X" as approximation
  eX <- strwidth("X", units = "inches")
  # 1.01 actually seems too wide for Helvetica.
  label_max_width <- 1.00 * label_max_width / eX
  if (verbose){
    message('I chose ', label_max_width, ' for the right margin.\n',
            'If my choice of margin is unsuitable for the label,\n',
            'you can use\n',
            '  right_margin = ', 
            '\nas a replacement for ', label_max_width, '\n',
            'It is my job to select a good margin; so please\n',
            'report any bad choices of mine as a bug.')
  }
  
  ## Need to check whether the texts will overlap
  
  if (geom == "bar"){
    if (missing(barwidth)){
      p <- 
        grplot(.plot.data) + 
        theme_hugh(base_family = text_family) + 
        ggplot2::geom_bar(ggplot2::aes(x = x, y = y, fill = fill), stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = text.label, 
                                        x = text.x,
                                        y = text.y, 
                                        colour = fill), 
                           na.rm = TRUE,
                           hjust = 0,
                           lineheight = 0.9,
                           family = text_family,
                           size = 20/(14/5),
                           fontface = "bold") 
    } else {
      p <- 
        grplot(.plot.data) + 
        theme_hugh(base_family = text_family) + 
        ggplot2::geom_bar(ggplot2::aes(x = x, y = y, fill = fill), stat = "identity", width = barwidth) +
        ggplot2::geom_text(ggplot2::aes(label = text.label, 
                                        x = text.x,
                                        y = text.y, 
                                        colour = fill), 
                           na.rm = TRUE,
                           hjust = 0,
                           lineheight = 0.9,
                           family = text_family,
                           size = 20/(14/5),
                           fontface = "bold") 
    }
    if (!missing(scale_x_args)){
      if (x_continuous){
        p <- p + do.call(ggplot2::scale_x_continuous, args = scale_x_args)
      } else {
        p <- p + do.call(ggplot2::scale_x_discrete, args = scale_x_args)
      }
    }
    
    if (!missing(scale_y_args)){
      p <- p + do.call(ggplot2::scale_y_continuous, args = scale_y_args)
    }
    
    if (!missing(coord_cartesian_args)){
      p <- p + do.call(ggplot2::coord_cartesian, args = coord_cartesian_args)
    }
    
    if (!missing(annotate_args)){
      p <- p + do.call(ggplot2::annotate, args = annotate_args)
    }
    
    if (!missing(theme_grattan.args)){
      p <- p + do.call(theme_hugh, theme_grattan.args)
    }

    if (missing(right_margin)){
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, label_max_width, 0.5, 0),
                                                       "lines"))
    } else {
      p <- p + ggplot2::theme(plot.margin = grid::unit(c(0.7, right_margin, 0.5, 0), 
                                                       "lines"))
    }
    
    if (!missing(theme.args)){
      p <- p + do.call(theme, theme.args)
    }
  } else {
    stop("You've asked for a geom which is not supported.")
  }
  grid::grid.newpage()
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)
}

