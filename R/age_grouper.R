#' Age grouper
#' 
#' @param age A numeric age (in years).
#' @param interval How big should the age range be. 25-34 means interval = 10.
#' @param min_age What is the upper bound of the lowest bracket? (\code{min_age = 25} means 'Under 25' will be the lowest bracket.)
#' @param max_age What is the lower bound of the highest bracket? (\code{max_age = 75} means '75+' will be the bracket.)
#' @param breaks Specify breaks manually.
#' @param labels Specify the labels manually.
#' @return An ordered factor giving age ranges (separated by hyphens) as specified. 
#' @examples 
#' age_grouper(42)
#' age_grouper(42, interval = 5, min_age = 20, max_age = 60)
#' @export

age_grouper <- function(age, 
                        interval = 10,
                        min_age = 25,
                        max_age = 75,
                        breaks = NULL,
                        labels = NULL){
  if (is.null(breaks)){
    if (!missing(labels) || !is.null(labels)){
      warning("breaks not specified, but labels is given (and will be ignored).")
    }
    
    cut(age, 
        breaks = c(-Inf,
                   seq(min_age, max_age, by = interval),
                   Inf), 
        labels = c(paste0("Below\n", min_age),
                   paste(seq(min_age, max_age - interval, by = interval),
                         seq(min_age + interval - 1, max_age - 1, by = interval),
                         sep = "-"), 
                   paste0(max_age, "+")), 
        right = FALSE,  # 25-34 not 26-35
        include.lowest = TRUE,
        ordered_result = TRUE)
  } else {
    if (!missing(interval) || !missing(min_age) || !missing(max_age)){
      stop("interval, min_age, max_age, can't be specified if breaks are.")
    }
    
    cut(age,
        breaks = breaks,
        labels = labels,
        right = FALSE,
        include.lowest = TRUE,
        ordered_result = TRUE)
  }
}
