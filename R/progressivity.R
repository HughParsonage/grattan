#' Compute the progressivity
#' @param income Pre-tax income.
#' @param tax Tax paid.
#' @param measure Currently, only "Reynolds-Smolensky" progressivity is calculated:
#' \deqn{G_Y - G_Z} where \eqn{G_Y} is the Gini coefficient of \code{income}
#' and \eqn{G_X} is the Gini coefficient of post-tax income.
#' @return The progressivity measure. Positive for progressive tax systems, and higher the
#' value the more progressive the system.
#' 
#' @examples
#' 
#' I <- c(10e3, 20e3, 50e3, 100e3, 150e3)
#' progressivity(I, 0.3 * I) # zero
#' progressivity(I, income_tax(I, "2017-18"))
#' 
#' @export
#' 

progressivity <- function(income,
                          tax,
                          measure = c("Reynolds-Smolensky", "Kakwani")) {
  measure <- match.arg(measure)
  switch(measure, 
         "Reynolds-Smolensky" = {
           post_tax_income <- income - tax
           Gy <- ineq::Gini(income, corr = FALSE, na.rm = FALSE)
           Gz <- ineq::Gini(post_tax_income, corr = FALSE, na.rm = FALSE)
           Gy - Gz
         },
         "Kakwani" = {
           .NotYetUsed(measure)
         })

}


