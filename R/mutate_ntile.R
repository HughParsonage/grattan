#' Add a column of ntiles to a data table
#' @param DT A \code{data.table}.
#' @param col The column name (quoted or unquoted) for which quantiles are desired.
#' @param n A positive integer, the number of groups to split \code{col}.
#' @param by,keyby Produce a grouped quantile column, as in \code{\link[data.table]{data.table}}.
#'   \code{keyby} will set a key on the result (\emph{i.e.} order by \code{keyby}).
#' @param new.col If not \code{NULL}, the name of the column to be added. 
#' If \code{NULL} (the default) a name will be inferred from \code{n}. 
#' (For example, \code{n = 100} will be \code{<col>Percentile}).
#' @param overwrite (logical, default: \code{TRUE}) If \code{TRUE} and
#' \code{new.col} already exists in \code{DT}, the column will be overwritten.
#' If \code{FALSE}, attempting to overwrite an existing column is an error.
#' @param check.na (logical, default: \code{FALSE}) If \code{TRUE}, \code{NA}s
#' in \code{DT[[col]]} will throw an error. If \code{NA}'s are present, the 
#' corresponding n-tile may take any value.
#' 
#' @examples
#' library(data.table)
#' DT <- data.table(x = 1:20, y = 2:1)
#' mutate_ntile(DT, "x", n = 10)
#' mutate_ntile(DT, "x", n = 5)
#' mutate_ntile(DT, "x", n = 10, by = "y")
#' mutate_ntile(DT, "x", n = 10, keyby = "y")
#' 
#' @return \code{DT} with a new integer column \code{new.col} containing the quantiles.
#' 
#' @export mutate_ntile



mutate_ntile <- function(DT,
                         col,
                         n,
                         by = NULL,
                         keyby = NULL,
                         new.col = NULL,
                         overwrite = TRUE,
                         check.na = FALSE) {
  if (length(n) != 1L) {
    stop("`length(n) = ", length(n), "`.", 
         "`n` must be a single positive whole number.")
  }
  if (!is.integer(n)) {
    if (!is.double(n)) {
      stop("`n` was type ", typeof(n), ".", 
           "`n` must be a single positive whole number.")
    }
    .n <- as.integer(n)
    if (.n != n) {
      stop("`n` was type double but `n != as.integer(n)`.", 
           "`n` must be a single positive whole number.")
    }
    n <- .n
  }
  
  if (missing(col)) {
    stop("Argument `col` is missing, with no default.")
  }
  
  .col <- as.character(substitute(col))
  if (is.null(new.col)) {
    suffix <- 
      switch(as.character(n), 
             "3" = "Terciles",
             "4" = "Quartile", 
             "5" = "Quintile",
             "6" = "Sextile",
             "7" = "Septile",
             "8" = "Octile",
             "10" = "Decile", 
             "12" = "DuoDecile",
             "16" = "Hexadecile",
             "20" = "Vigintile",
             "100" = "Percentile", 
             "1000" = "Permilles",
             stop("`n = ", n, "` and new.col is NULL, ",
                  "but no default column suffix is supported.", 
                  "Supply a column name using `new.col`."))
    new.col <- paste0(.col, suffix)
  }
  
  if (!overwrite && new.col %chin% names(DT)) {
    stop("`DT` already has a column '", new.col, "' ",
         "and `overwrite = TRUE`, so stopping, as requested.",
         "Either supply a different name to `new.col` or set `overwrite = TRUE` ",
         "to allow the new column.")
  }
  
  # Needs to be after new.col construction to provide early return
  if (n <= 1L) {
    if (n == 1L) {
      message("`n = 1L` so adding a column of 1s. (This is an unlikely value for ntiles.)")
      if (is.data.table(DT)) {
        return(DT[, (new.col) := 1L][])
      } else {
        DT[[new.col]] <- 1L
        return(DT)
      }
    } else {
      stop("`n = ", n, "`.",
           "`n` must be a single positive whole number.")
    }
  }
  
  
  if (not_DT <- !is.data.table(DT)) {
    input_class <- class(DT)
    if (!is.data.frame(DT)) {
      stop("`DT` was a ", class(DT)[1], " but must be a data.frame.")
    }
    DT <- as.data.table(DT)
  }
  
  definitely_sorted <- function(ddt, nom, check_na) {
    if (haskey(ddt) && key(ddt)[1] == nom) {
      return(TRUE)
    }
    x <- .subset2(ddt, nom)
    if (anyNA(x)) {
      if (check_na) {
        stop("`check.na = TRUE` yet `DT[['", nom, "']]` ", 
             "so stopping, as requested.")
      }
      return(FALSE)
    }
    !is.unsorted(x)
  }
  
  
  if (definitely_sorted(DT, .col, check.na)) {
    if (is.null(by) && is.null(keyby)) {
      DT[, (new.col) := .ntile(.SD[[1L]], n, check.na = check.na),
         .SDcols = c(.col)]
    }
    if (!is.null(by)) {
      if (!is.null(keyby)) {
        stop("`by` is NULL, yet `keyby` is NULL too. ", 
             "Only one of `by` and `keyby` may be provided.")
      }
      
      DT[, (new.col) := .ntile(.SD[[.col]], n, check.na = check.na),
         .SDcols = c(.col),
         by = c(by)]
    }
    if (!is.null(keyby)) {
      DT[, (new.col) := .ntile(.SD[[.col]], n, check.na = check.na),
         .SDcols = c(.col),
         keyby = c(keyby)]
      setkeyv(DT, keyby)
    }
    
  } else {
    if (is.null(by) && is.null(keyby)) {
      DT[, (new.col) := weighted_ntile(.SD[[.col]], n = n),
         .SDcols = c(.col)]
    }
    if (!is.null(by)) {
      if (!is.null(keyby)) {
        stop("`by` is NULL, yet `keyby` is NULL too. ", 
             "Only one of `by` and `keyby` may be provided.")
      }
      
      DT[, (new.col) := weighted_ntile(.SD[[.col]], n = n),
         .SDcols = c(.col),
         by = c(by)]
    }
    if (!is.null(keyby)) {
      DT[, (new.col) := weighted_ntile(.SD[[.col]], n = n),
         .SDcols = c(.col),
         keyby = c(keyby)]
      return(setkeyv(DT, keyby)[])
    }
    
  }
  if (not_DT) {
    setDF(DT)
    if (!identical(input_class, "data.frame")) {
      class(DT) <- input_class
    }
  }
  DT[]
}

.ntile <- function(x, n, check.sorted = FALSE, check.na = FALSE) {
  if (check.sorted && is.unsorted(x)) {
    stop("`x` must be already sorted.")
  }
  if (check.na && anyNA(x)) {
    stop("`anyNA(x)` is TRUE.")
  }
  
  as.integer(n * {seq_along(x) - 1L} / length(x) + 1L)
}
