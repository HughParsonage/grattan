#' Beneficiary tax offset
#' @param benefit_amount The amount of Tax Offsetable benefit received by the taxpayer during the income year.
#' @param fy.year The income year. If supplied, selects the legislated
#' coefficients and the threshold at the end of the lowest positive-rate band.
#' @param rate1 The coefficient in Division 2, section 13(2) of the Income Tax Assessment (1936 Act) Regulation 2015
#' (the regulations).
#' @param benefit_threshold The amount of benefits above which the offset applies.
#' @param tax_threshold The \emph{threshold at the upper conclusion of the lowest marginal tax rate} in the
#' words of the section 13(3) of the regulations.
#' @param rate2 The second coefficient in section 13(3) of the regulations.

#' @return The beneficiary tax offset. 
#' @export

bto <- function(benefit_amount,
                fy.year = NULL,
                rate1 = 0.15,
                benefit_threshold = 6000,
                tax_threshold = 37000,
                rate2 = 0.15) {
  if (!length(benefit_amount)) {
    return(numeric())
  }
  if (!is.null(fy.year)) {
    yr <- fy::fy2yr(fy.year)
    if (anyNA(yr) || any(yr < 1997)) {
      stop("fy.year must be 1996-97 or later.")
    }
    previous <- match(yr, fy::fy2yr(.subset2(bto_tbl, "fy_year")))
    # Since 2005-06 the 15% coefficients and $6,000 allowance are fixed
    # by regulation; the next-band threshold still follows the tax year.
    if (missing(rate1)) {
      rate1 <- ifelse(yr <= 2006, .subset2(bto_tbl, "lowest_marginal_rate")[previous], 0.15)
    }
    if (missing(benefit_threshold)) {
      benefit_threshold <- ifelse(yr <= 2006, .subset2(bto_tbl, "tax_free_threshold")[previous], 6000)
    }
    if (missing(rate2)) {
      rate2 <- ifelse(yr <= 2006, .subset2(bto_tbl, "coefficient_abv_next_threshold")[previous], 0.15)
    }
    if (missing(tax_threshold)) {
      tax_threshold <- vapply(yr, function(y) brack_by_year(as.integer(y), 3L), integer(1))
    }
  }
  prohibit_vector_recycling(benefit_amount, rate1, benefit_threshold, tax_threshold, rate2)
  # Regulation 13(4)-(5): floor benefits before calculating and rounding up
  # the offset. The order matters for benefit amounts containing cents.
  benefit_amount <- floor(benefit_amount)
  # Prescribed coefficients are whole percentages. Multiply in percentage
  # points before dividing to preserve exact whole-dollar boundaries.
  first <- benefit_amount - benefit_threshold
  second <- pmax(benefit_amount - tax_threshold, 0)
  points1 <- round(100 * rate1)
  points2 <- round(100 * rate2)
  exact <- rate1 == points1 / 100 & rate2 == points2 / 100
  amount <- ifelse(exact & is.finite(first + second),
                   (points1 * first + points2 * second) / 100,
                   rate1 * first + rate2 * second)
  pmax(ceiling(amount), 0)
}
