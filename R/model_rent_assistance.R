#' Model Rent Assistance
#' 
#' @param sample_file A sample file having the same variables as the data.frame in the example.
#' @param .Prop_rent_paid_by_RA The proportion of the rent above the minimum threshold paid by rent assistance. 
#' @param Max_rate If not \code{NULL}, a numeric vector indicating for each individual the maximum rent assistance payable.
#' @param Min_rent If not \code{NULL}, a numeric vector indicating for each individual the minimum fortnightly rent above which rent assistance is payable. \code{max_rate} and \code{min_rent}
#' @param return. What should the function return? One of \code{tax}, \code{sample_file}, or \code{sample_file.int}. 
#' If \code{tax}, the tax payable under the settings; if \code{sample_file}, the \code{sample_file},
#' but with variables \code{tax} and possibly \code{new_taxable_income}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_tax} is coerced to integer.

#' @export 
#' 
#' @examples
#' sample<- CJ(rent = 1:500, n_dependants = 0:3, has_partner = 0:1 > 0, is_homeowner = 0:1 > 0, lives_in_sharehouse = 0:1 > 0) 
#' model_rent_assistance(sample, .Prop_rent_paid_by_RA = 0.75, Max_rate = 500, Min_rent = 100)

model_rent_assistance <- function(sample_file,
                                  baseline_fy = NULL,
                                  baseline_Date = NULL,
                                  .Prop_rent_paid_by_RA,
                                  Max_rate = NULL,
                                  Min_rent = NULL,
                                  return. = c("new_ra", "sample_file", "sample_file.int")) {
  
  #check sample file has correct format
  stopifnot(is.data.table(sample_file))
  cols_required <- c("rent",  "n_dependants", "has_partner", "is_homeowner", "lives_in_sharehouse")
  if (!all(cols_required %chin% colnames(sample_file))) {
    absent_cols <- setdiff(cols_required, colnames(sample_file))
    stop("`sample_file` lacked the following required columns:\n\t",
         paste0(absent_cols, collapse = "\n\t"), ".\n")
  }
  
  #actual calculation
  Rent <- sample_file[['rent']]
  N_dependants <- sample_file[['n_dependants']]
  Has_partner <- sample_file[['has_partner']]
  Is_homeowner <- sample_file[['is_homeowner']]
  Lives_in_sharehouse <- sample_file[['lives_in_sharehouse']]
  
  baseline_ra <- rent_assistance(fortnightly_rent = Rent, 
                            fy.year = baseline_fy,
                            Date = baseline_Date,
                            n_dependants = N_dependants,
                            has_partner = Has_partner,
                            is_homeowner = Is_homeowner,
                            lives_in_sharehouse = Lives_in_sharehouse)
  
  new_ra <- rent_assistance(fortnightly_rent = Rent, 
                            n_dependants = N_dependants,
                            has_partner = Has_partner,
                            is_homeowner = Is_homeowner,
                            lives_in_sharehouse = Lives_in_sharehouse,
                            .prop_rent_paid_by_RA = .Prop_rent_paid_by_RA,
                            max_rate = Max_rate,
                            min_rent = Min_rent)
  
  switch(return.,
         "ra" = new_ra,
         "sample_file" = set(sample_file, j = "new_ra", value = new_ra),
         "sample_file.int" = set(sample_file, j = "new_ra", value = as.integer(new_ra)))
  
}
