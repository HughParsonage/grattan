

Sys.Date <- function(mock = NULL) {
  if (is_testing()) {
    return(as.Date("2022-05-26"))
  }
  if (is.null(mock)) {
    return(as.Date(as.POSIXct(Sys.time())))
  }
  return(as.Date(mock))
}
