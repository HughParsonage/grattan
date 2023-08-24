#' Set offsets
#' @description Create parameters for tax offsets.
#' @param offset_1st \code{integer(1)} The offset available for zero income.
#' @param thresholds \code{integer(N)} An sorted integer vector, the thresholds
#' above which each taper applies.
#' @param tapers \code{double(N)} The tapers above each \code{threshold}. Positive
#' tapers mean that the offset reduces for every dollar above the corresponding
#' threshold.
#' @param refundable \code{bool(1)} If \code{FALSE}, the default, offsets are 
#' non-refundable, meaning that the offset cannot reduce the tax below zero.
#' 
#' @param ... A set of offsets created by \code{set_offset}. May not exceed
#' \code{the_MAX_N_OFFSETN()}.
#' @param yr \code{NULL / integer(1)} If \code{NULL}, only the offsets created
#' by \code{...} are used. Otherwise, inherits offsets (such as LITO and LMITO)
#' from the corresponding year.
#' @param lito_max_offset,lito_taper,lito_min_bracket,lito_multi deprecated
#' arguments to adjust (single-threshold) LITO.
#' 
#' @return 
#' \describe{
#' \item{\code{set_offset}}{A list of four elements, \code{offset_1st}, 
#' \code{thresholds}, \code{tapers}, \code{refundable}.}
#' \item{\code{set_offsets}}{A list of lists created by \code{set_offset}.}
#' \item{\code{the_MAX_N_OFFSETN}}{The maximum number of offsets that may be used.}
#' }
#' 
#' @export

set_offset <- function(offset_1st = integer(1),
                       thresholds = integer(), 
                       tapers = double(), 
                       refundable = logical(1)) {
  checkmate::assert_integerish(offset_1st)
  stopifnot(length(thresholds) == length(tapers),
            is.numeric(thresholds), 
            is.numeric(tapers))
  check_TF(refundable)
  if (is.unsorted(thresholds)) {
    stop("`thresholds = ", thresholds, "` was unsorted.")
  }
  list(offset_1st = as.integer(offset_1st),
       thresholds = as.integer(thresholds),
       tapers = tapers, 
       refundable = refundable)
}


#' @rdname set_offset
#' @export
set_offsets <- function(..., 
                        yr = NULL, 
                        lito_max_offset = NULL,
                        lito_taper = NULL,
                        lito_min_bracket = NULL,
                        lito_multi = NULL) {
  if (is.null(yr)) {
    return(list(...))
  }
  stopifnot(is.integer(yr))
  DefaultOffsets <- .Call("C_yr2Offsets", yr, PACKAGE = packageName())
  if (yr > 1994 && length(DefaultOffsets)) {
    lito_not_NULL <- FALSE
    # LITO only available after 1994, we assume LITO is always first
    if (!is.null(lito_max_offset)) {
      lito_not_NULL <- TRUE
      DefaultOffsets[[1]]$offset_1st <- as.integer(lito_max_offset)
    }
    if (!is.null(lito_taper)) {
      lito_not_NULL <- TRUE
      DefaultOffsets[[1]]$tapers <- lito_taper
    }
    if (!is.null(lito_min_bracket)) {
      lito_not_NULL <- TRUE
      DefaultOffsets[[1]]$thresholds <- as.integer(lito_min_bracket)
    }
    if (lito_not_NULL) {
      message("lito_ arguments are deprecated. Use set_offsets instead.")
    }
  }
  if (missing(..1)) {
    return(DefaultOffsets)
  }
  Dots <- list(...)
  n <- length(DefaultOffsets)
  for (j in seq_along(Dots)) {
    DefaultOffsets[[n + j]] <- Dots[[j]]
  }
  DefaultOffsets
}

#' @rdname set_offset
#' @export
the_MAX_N_OFFSETN <- function() {
  .Call("C_MAX_N_OFFSETN", NULL, PACKAGE = packageName())
}


Offset <- function(x, y, a, m) {
  .Call("COffset", x, y, a, m, PACKAGE = packageName())
}






multiOffset <- function(x, Offsets = set_offsets(), nThread = getOption("grattan.nThread", 1L)) {
  .Call("C_multiOffset", x, Offsets, nThread, PACKAGE = packageName())
}

nOffset_upper_threshold <- function(List = set_offsets(), j = 1L) {
  stopifnot(is.list(List), is.integer(j))
  .Call("Ctest_nOffset_upper_threshold", List, j, PACKAGE = packageName())
}
