#' chart_data
#' 
#' convenience function for knitr chunks
#' 
#' @param .data A data frame suitable for \code{readr::write_csv}.
#' @return If knitting, writes a csv of its named as the current chunk label. Otherwise, returns \code{.data} invisibly.
#' @usage Use this in a knitr chunk that produces a chart to get the chart data 

chart_data <- function(.data){
  current_chunk_label <- knitr::opts_current$get(name = "label")
  if(!is.null(current_chunk_label)){  # i.e. we are knitting
    if(!exists("atlas")){
      atlas <- knitr::opts_chunk$get(name = "fig.path")
    }
    readr::write_csv(x = .data, path = file.path(atlas, paste0(current_chunk_label, ".csv")))
  } else {
    invisible(.data)
  }
}