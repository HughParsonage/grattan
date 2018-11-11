#' Age grouper
#' 
#' @param age A numeric age (in years).
#' @param interval How big should the age range be. 25-34 means interval = 10.
#' @param min_age What is the upper bound of the lowest bracket? (\code{min_age = 25} means 'Under 25' will be the lowest bracket.)
#' @param max_age What is the lower bound of the highest bracket? (\code{max_age = 75} means '75+' will be the bracket.)
#' @param breaks Specify breaks manually.
#' @param labels Specify the labels manually.
#' @param below String giving the prefix for the lowest bin. (Only applicable
#' if \code{breaks} and \code{labels} are \code{NULL}.)
#' @param exp_min_age,exp_max_age Integers specifying the lowest/highest expected 
#' age in \code{age}. If any values fall outside this range, ages will still work
#' though perhaps slow when \code{length(age) >> threshold}.
#' @param threshold An integer, the minimum length at which the calculation will
#' be accelerated.
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
                        labels = NULL,
                        below = "Below\n",
                        exp_min_age = 1L,
                        exp_max_age = 100L,
                        threshold = 10e3L) {
  
  if (is.null(breaks)){
    if (!is.null(labels)) {
      warning("breaks not specified, but labels is given (and will be ignored).")
    }
    
    if (length(age) > threshold &&
        min(age) >= exp_min_age &&
        max(age) <= exp_max_age) {
      
      i <- age - exp_min_age + 1L
      
      ans <- 
        age_grouper(exp_min_age:exp_max_age,
                    interval = interval,
                    min_age = min_age,
                    max_age = max_age,
                    breaks = breaks,
                    labels = labels,
                    threshold = Inf,
                    below = below)[i]
      return(ans)
    }
    
    cut(age, 
        breaks = c(-Inf,
                   seq(min_age, max_age, by = interval),
                   Inf), 
        labels = c(paste0(below, min_age),
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
