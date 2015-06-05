#' chart_data
#' 
#' convenience function for knitr chunks

chart_data <- function(.data){
  current_chunk_label <- opts_current$get(name = "label")
  readr::write_csv(x = .data, path = paste0("./figure/", current_chunk_label, ".csv"))
}