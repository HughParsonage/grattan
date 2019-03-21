#' Attach a 'taxstats' package
#' @name require_taxstats
#' @description Used in lieu of simply \code{library(taxstats)} to 
#' handle cases where it is not installed, but should not be installed
#' to the user's default library (as during CRAN checks).
#' @return \code{TRUE}, invisibly, for success. Used for its side-effect: attaching the taxstats 
#' package
#' 
#' @export require_taxstats 


# nocov start
require_taxstats <- function() {
  .require_taxstats("taxstats")
}

#' @rdname require_taxstats
#' @export require_taxstats1516
require_taxstats1516 <- function() {
  .require_taxstats("taxstats1516")
}

.require_taxstats <- function(pkg = c("taxstats", "taxstats1516")) {
  pkg <- match.arg(pkg)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (is.null(temp_lib <- getOption("grattan.taxstats.lib"))) {
      temp_lib <- tempfile()
    }
    hutils::provide.dir(temp_lib)
    install_taxstats(pkg, lib = temp_lib)
    library(pkg, character.only = TRUE, lib.loc = temp_lib)
    return(invisible(TRUE))
  }
  library(pkg, character.only = TRUE)
  invisible(TRUE)
}
# nocov end
