#' AWOTE
#' @description Adult weekly ordinary-time earnings
#' @param Date,fy.year When the AWOTE is desired.
#' @param isMale (logical, default: \code{NA}) \code{TRUE} for male weekly earnings, 
#' \code{FALSE} for female, \code{NA} for the weekly earnings of both sexes.
#' @param isAdult (logical, default: \code{TRUE}) Use adult weekly earnings?
#' @param rollDate How should the \code{Date} be joined to the source data?
#' Passed to \code{data.table}.
#' @param isOrdinary Use ordinary weekly earnings?
#' 
#' @examples
#' awote()  # Current AWOTE
#' 
#' 
#' @export awote

awote <- function(Date = NULL,
                  fy.year = NULL,
                  rollDate = "nearest",
                  isMale = NA,
                  isAdult = TRUE,
                  isOrdinary = TRUE) {
  if (!is.logical(isMale)) {
    stop("`isMale` was type ", typeof(isMale), ", but must be a logical.")
  }
  if (!is.logical(isAdult)) {
    stop("`isAdult` was type ", typeof(isAdult), ", but must be a logical.")
  }
  if (anyNA(isAdult)) {
    stop("`isAdult` contains NAs. Impute these values.")
  }
  if (!is.logical(isOrdinary)) {
    stop("`isOrdinary` was type ", typeof(isOrdinary), ", but must be a logical.")
  }
  if (anyNA(isOrdinary)) {
    stop("`isOrdinary` contains NAs. Impute these values.")
  }
 
  if (is.null(Date) && is.null(fy.year)) {
    Date <- Sys.Date()
    message("`Date` and `fy.year` both NULL so using `Date = ", as.character(Date), "`.")
  } else if (!is.null(fy.year)) {
    fy_year <- validate_fys_permitted(fy.year,
                                      deparsed = "fy.year",
                                      min.yr = 1994L,
                                      max.yr = 2018L)
    return(awote_fy(fy_year = fy_year,
                    isMale = isMale,
                    isAdult = isAdult,
                    isOrdinary = isOrdinary))
    
  }
  
  max.length <-
    prohibit_vector_recycling.MAXLENGTH(Date,
                                        isMale,
                                        isAdult,
                                        isOrdinary)
  
  input <- data.table(Date = as.Date(Date),
                      isMale = isMale,
                      isAdult = isAdult,
                      isOrdinary = isOrdinary) %>%
    setkeyv(key(AWOTE_by_Date_isMale_isOrdinary_isAdult))
  
  AWOTE_by_Date_isMale_isOrdinary_isAdult %>%
    .[input, roll = rollDate] %>%
    .subset2("AWOTE")
}

awote_fy <- function(fy_year, isMale, isAdult, isOrdinary) {
  fy_year <- validate_fys_permitted(fy_year)
  input <- data.table(fy_year, isMale, isAdult, isOrdinary)
  setkeyv(input, names(input))
  Date <- AWOTE <- NULL
  AWOTE_by_Date_isMale_isOrdinary_isAdult %>%
    .[, "fy_year" := date2fy(Date)] %>%
    .[, .(AWOTE = mean(AWOTE)), 
      keyby = .(fy_year,
                isMale,
                isOrdinary,
                isAdult)] %>%
    .[input, on = c(key(.))] %>%
    .subset2("AWOTE")
}
