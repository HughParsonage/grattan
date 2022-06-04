

Offset <- function(x, y, a, m) {
  .Call("COffset", x, y, a, m, PACKAGE = packageName())
}

memOffset <- function(x, offset_1st = 445L, 
                      thresholds = 37000L,
                      tapers = 0.015) {
  .Call("C_moffset", x, offset_1st, thresholds, tapers, 
        PACKAGE = packageName())
}

test_currency <- function(x, taper) {
  .Call("TestCurrency", as.integer(x), as.double(taper), PACKAGE = packageName())
}

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

multiOffsets <- function(x, Offsets = set_offsets(), nThread = getOption("grattan.nThread", 1L)) {
  .Call("C_multiOffset", x, Offsets, nThread, PACKAGE = packageName())
}

nOffset_upper_threshold <- function(List = set_offsets(), j = 1L) {
  stopifnot(is.list(List), is.integer(j))
  .Call("Ctest_nOffset_upper_threshold", List, j, PACKAGE = packageName())
}
