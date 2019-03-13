#' Install 'taxstats' files
#' @description The taxstats packages provide the sample files as released by the ATO. These packages are used for testing, but are not available through CRAN as they are too large.
#' @param pkg The package to install such as \code{"taxstats"} or \code{"taxstats1516"}.
#' @param ... Arguments passed to \code{\link[utils]{install.packages}}.
#' @export

install_taxstats <- function(pkg = c("taxstats"), ...) {
  if (!missing(..1)) {
    dots <- list(...)
    if ("lib" %in% names(dots)) {
      if (!requireNamespace("data.table", lib.loc = dots$lib)) {
        utils::install.packages("data.table", lib = dots$lib, ...)
      }
    }
  }
  utils::install.packages(pkgs = c(pkg),
                          repos = "https://hughparsonage.github.io/tax-drat",
                          type = "source", 
                          ...)
}

