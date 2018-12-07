#' AWOTE
#' @description Adult weekly ordinary-time earnings
#' @param Date,fy.year When the AWOTE is desired.
#' @param Sex (If \code{NULL}, the AWOTE of all persons is used.) Otherwise, 
#' \code{Male} or \code{Female}.
#' @param isAdult (logical, default: \code{TRUE}) Use adult weekly earnings?
#' @param rollDate How should the \code{Date} be joined to the source data?
#' Passed to \code{data.table}.
#' @param isOrdinary Use ordinary weekly earnings?

awote <- function(Date = NULL,
                  fy.year = NULL,
                  Sex = NULL,
                  isAdult = TRUE,
                  rollDate = "nearest",
                  isOrdinary = TRUE) {
  if (is.null(Date) && is.null(fy.year)) {
    Date <- Sys.Date()
    message("`Date` and `fy.year` both NULL so using `Date = ", as.character(Date), "`.")
  } else if (!is.null(fy.year)) {
    fy_year <- validate_fys_permitted(fy.year,
                                      deparsed = "fy.year",
                                      min.yr = 1994L,
                                      max.yr = 2018L)
    return(awote_fy(fy_year = fy_year,
                    Sex = Sex,
                    isAdult = isAdult,
                    isOrdinary = isOrdinary))
    
  }
  
  if (is.null(Sex)) {
    isMale <- NA
  } else {
    if (anyNA(Sex)) {
      warning("`Sex` contains missing values. These will be missing in the result.")
      isMale[complete.cases(Sex)] <- Sex[complete.cases(Sex)] %chin% c("Male", "male", "M", "m")
    } else {
      isMale <- Sex %chin% c("Male", "male", "M", "m")
    }
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
