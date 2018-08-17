


# Provide error function that can refer to the calling function.
stopn <- function(..., n = -sys.nframe()) {
  error_message <- paste0(..., collapse = "")
  condition <- function(subclass, message, call = sys.call(-1), ...) {
    structure(
      class = c(subclass, "condition"),
      list(message = message, call = call),
      ...
    )
  }
  custom_stop <- function(subclass, message, call = sys.call(n - 1L), ...) {
    ER <- condition(c("my_error", "error"), error_message, call = call, ...)
    stop(ER)
  }
  custom_stop()
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

