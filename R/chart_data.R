#' chart_data
#' 
#' @description convenience function for knitr chunks
#' 
#' @param .data A data frame suitable for \code{readr::write_csv}.
#' @param warnOnError The function is designed not to error (to preserve portability). If the function is required to work, setting to \code{TRUE}.
#' @return If knitting, writes a csv of its named as the current chunk label. Otherwise, returns \code{.data} invisibly.
#' @note  Use this in a knitr chunk that produces a chart to get the chart data.
#' @export 

chart_data <- function(.data, warnOnError = FALSE){
  tryCatch({
    current_chunk_label <- knitr::opts_current$get(name = "label")
    if (!is.null(current_chunk_label)){  # i.e. we are knitting
      atlas <- gsub("/$", "", knitr::opts_chunk$get(name = "fig.path"))
      readr::write_csv(x = .data, path = file.path(atlas, paste0(current_chunk_label, ".csv")))
    } else {
      invisible(.data)
    }}, 
    error = function(cond) {if (warnOnError) warning("chart_data failing.") else invisible(.data)})
}