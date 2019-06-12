


# Provide error function that can refer to the calling function.
stopn <- function(..., n = sys.nframe()) {
  stop(..., call. = FALSE)
}


# Error and provide calling function:
stop_via <- function(..., FUNCTION) {
  if (missing(FUNCTION)) stop(...)
  error_message <- paste0(..., collapse = "")
  caller <- as.name(FUNCTION)
  stop(structure(class = c("error", "condition"),
                 list(message = error_message,
                      call = caller)))
}

