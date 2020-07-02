#' Install 'taxstats' files
#' @description The taxstats packages provide the sample files as released by the
#'  ATO. These packages are used for testing, but are not available through CRAN 
#'  as they are too large.
#' @param pkg The package to install such as \code{"taxstats"} or \code{"taxstats1516"}.
#' @param ... Arguments passed to \code{\link[utils]{install.packages}}.
#' @export

install_taxstats <- function(pkg = c("taxstats"), ...) {
  if (!identical(Sys.getenv("R_GRATTAN_BUILD_MAIN_VIGNETTE"), "true") ||
      !identical(Sys.getenv("R_GRATTAN_ALLOW_TAXSTATS"), "true")) {
    message("Unable to install taxstats. Set both of the following environment variables\n\t", 
            "Sys.setenv('R_GRATTAN_BUILD_MAIN_VIGNETTE' = 'true')\n", 
            "# and\n\t",
            "Sys.setenv('R_GRATTAN_ALLOW_TAXSTATS' = 'true')\n")
    return(NULL)
  }
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

