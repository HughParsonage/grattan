#' Model Rent Assistance
#' 
#' @param sample_file A sample file having the same variables as the data.frame in the example.
#' @param .Prop_rent_paid_by_RA The proportion of the rent above the minimum threshold paid by rent assistance. 
#' @param calc_baseline_ra (logical, default: \code{TRUE}) Should the income tax in \code{baseline_fy} or \code{baseline_Date} be included as a column in the result?
#' @param baseline_fy,baseline_Date (character) The financial year/date over which the baseline rent assistance is to be calculated. Only one can be provided.
#' @param Per Specifies the timeframe in which payments will be made. Can either take value "fortnight" or "annual".
#' @param Max_rate If not \code{NULL}, a numeric vector indicating for each individual the maximum rent assistance payable.
#' @param Min_rent If not \code{NULL}, a numeric vector indicating for each individual the minimum fortnightly rent above which rent assistance is payable. \code{max_rate} and \code{min_rent}
#' @param return. What should the function return? One of \code{tax}, \code{sample_file}, or \code{sample_file.int}. 
#' If \code{tax}, the tax payable under the settings; if \code{sample_file}, the \code{sample_file},
#' but with variables \code{tax} and possibly \code{new_taxable_income}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_tax} is coerced to integer.

#' @export 
#' 
#' @examples
#' 
#' library(data.table)
#' sample <-
#'   CJ(rent = 1:500,
#'      n_dependants = 0:3,
#'      has_partner = 0:1 > 0,
#'      is_homeowner = 0:1 > 0,
#'      lives_in_sharehouse = 0:1 > 0)
#' model_rent_assistance(sample,
#'                       baseline_fy = "2018-19",
#'                       .Prop_rent_paid_by_RA = 0.75,
#'                       Max_rate = 500,
#'                       Min_rent = 100)

model_rent_assistance <- function(sample_file,
                                  baseline_fy = NULL,
                                  baseline_Date = NULL,
                                  Per = "fortnight",
                                  .Prop_rent_paid_by_RA = NULL,
                                  Max_rate = NULL,
                                  Min_rent = NULL,
                                  calc_baseline_ra = TRUE,
                                  return. = c("sample_file", "new_ra", "sample_file.int")) {
  return. <- match.arg(return.)
  
  if (!XOR(is.null(baseline_fy), is.null(baseline_Date))) {
    if (is.null(baseline_fy) && is.null(baseline_Date)) {
      stop("Neither `baseline_fy` nor `baseline_Date` was provided. ", 
           "Provide `baseline_fy` xor `baseline_Date`.")
    } else {
      stop("`baseline_fy` is NULL, yet `baseline_Date` is also NULL. ",
           "Only one of `baseline_fy` and `baseline_Date` may be provided.")
    } 
  }
  if (is.null(.Prop_rent_paid_by_RA) || is.null(Max_rate) || is.null(Min_rent)) {
    stop("need all of `.Prop_rent_paid_by_RA`, `Max_rate` and `Min_rent`.")
  }
  if (!is.numeric(.Prop_rent_paid_by_RA)) {
    stop("`.Prop_rent_paid_by_RA` was type ", typeof(.Prop_rent_paid_by_RA),
         ", but must be numeric.")
  }
  if (!is.numeric(Max_rate)) {
    stop("`Max_rate` was type ", typeof(Max_rate),
         ", but must be numeric.")
  }
  if (!is.numeric(Min_rent)) {
    stop("`Min_rent` was type ", typeof(Min_rent),", but must be numeric.")
  }
  
  check_TF(calc_baseline_ra)
 
  
  
  # Check sample file has correct format
  if (!is.data.frame(sample_file)) {
    stop("`sample_file` was a ", class(sample_file)[1],
         ", but must be a data.frame.")
  }
  
  # Effectively copy  
  sample_file <- as.data.table(sample_file)

  cols_required <- c("rent",  "n_dependants", "has_partner", "is_homeowner", "lives_in_sharehouse")
  if (!all(cols_required %chin% names(sample_file))) {
    absent_cols <- setdiff(cols_required, names(sample_file))
    stop("`sample_file` lacked the following required columns:\n\t",
         paste0(absent_cols, collapse = "\n\t"), ".\n")
  }
  
  # Actual calculation
  Rent <- sample_file[['rent']]
  N_dependants <- sample_file[['n_dependants']]
  Has_partner <- sample_file[['has_partner']]
  Is_homeowner <- sample_file[['is_homeowner']]
  Lives_in_sharehouse <- sample_file[['lives_in_sharehouse']]
  
  if (calc_baseline_ra && return. != "new_ra") {
    baseline_ra <- rent_assistance(fortnightly_rent = Rent, 
                                   fy.year = baseline_fy,
                                   Date = baseline_Date,
                                   per = Per,
                                   n_dependants = N_dependants,
                                   has_partner = Has_partner,
                                   is_homeowner = Is_homeowner,
                                   lives_in_sharehouse = Lives_in_sharehouse)
    set(sample_file,
        j = "baseline_ra",
        value = switch(return.,
                       "sample_file" = baseline_ra,
                       "sample_file.int" = as.integer(baseline_ra)))
  }
  
  ra <- rent_assistance(fortnightly_rent = Rent, 
                        per = Per,
                        .prop_rent_paid_by_RA = .Prop_rent_paid_by_RA,
                        max_rate = Max_rate,
                        min_rent = Min_rent)
  
  switch(return.,
         "new_ra" = ra[],
         "sample_file" = set(sample_file, j = "new_ra", value = ra)[],
         "sample_file.int" = set(sample_file, j = "new_ra", value = as.integer(ra))[])
  
}
