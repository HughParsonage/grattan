
# Functions for using during testing

skip_on_circleci <- function(node = NULL) {
  if (requireNamespace("testthat", quietly = TRUE)) {
    if (identical(Sys.getenv("CIRCLECI"), "true")) {
      if (is.null(node)) {
        testthat::skip("On Circle-CI")
      } else if (identical(Sys.getenv("CIRCLECI_NODE_INDEX"), as.character(node))) {
        testthat::skip(paste0("On Circle-CI node != ", as.character(node)))
      }
    }
  }
  invisible(TRUE)
}

