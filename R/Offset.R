

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
  list(offset_1st = offset_1st,
       thresholds = thresholds,
       tapers = tapers, 
       refundable = refundable)
}

set_offsets <- function(...) {
  list(...)
}

multiOffsets <- function(x, Offsets = set_offsets(), nThread = getOption("grattan.nThread", 1L)) {
  .Call("C_multiOffset", x, Offsets, nThread, PACKAGE = packageName())
}

nOffset_upper_threshold <- function(List = set_offsets(), j = 1L) {
  stopifnot(is.list(List), is.integer(j))
  .Call("Ctest_nOffset_upper_threshold", List, j, PACKAGE = packageName())
}
