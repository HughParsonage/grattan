#' Low Income Tax Offset
#' @name lito
#' @description The Low Income Tax Offset (LITO) is a non-refundable tax offset to reduce ordinary personal income tax for low-income earners.
#' @param input A keyed data.table containing the financial year and the input of every observation for which the LITO should be calculated.
#' The input must have the following structure. \strong{The structure will not be checked.}
#' \describe{
#' \item{fy_year}{The financial year the LITO parameters should be obtained. This must be the key of the data.table.}
#' \item{income}{The Taxable Income of the individual.}
#' \item{ordering}{An integer sequence from 1 to \code{nrow(input)} which will be the order of the output.}
#' }
#' @param income Income of taxpayer
#' @param max_lito The maximum LITO available.
#' @param lito_taper The amount by which LITO should be shaded out or reduced for every additional dollar of taxable income.
#' @param min_bracket The income at which the \code{lito_taper} applies.
#' @return For \code{.lito}, the a numeric vector equal to the offset for each income and each financial year in \code{input}. 
#' For \code{lito}, a numeric vector equal to the offset for each income given the LITO parameters. 
#' @export lito
NULL

#' @rdname lito
.lito <- function(input){
  income <- NULL
  lito_tbl[input] %>%
    .[,lito := pminV(pmaxC(max_lito - (income - min_bracket) * lito_taper, 0),
                     max_lito)] %>%
    setorderv("ordering") %>%
    .[["lito"]]
}


#' @rdname lito
lito <- function(income, 
                 max_lito = 445, 
                 lito_taper = 0.015, 
                 min_bracket = 37000){
  if (length(max_lito) == 1L){
    pminC(pmaxC(max_lito - (income - min_bracket) * lito_taper, 0),
          max_lito)
  } else {
    # Need to guard against unequal length vectors passed to pminV. In particular
    # there is a danger that max_lito will be single, but income won't.
    prohibit_unequal_length_vectors(income, max_lito, lito_taper, min_bracket)
    pminV(pmaxC(max_lito - (income - min_bracket) * lito_taper, 0),
          max_lito)
  }
}


