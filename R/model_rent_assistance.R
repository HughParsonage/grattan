#' Model Rent Assistance
#' 
#' @export 
#' 
#' @examples
#' sample<- CJ(rent = 1:500, n_dependants = 0:3, has_partner = 0:1 > 0, is_homeowner = 0:1 > 0, lives_in_sharehouse = 0:1 > 0) 
#' model_rent_assistance(sample, .Prop_rent_paid_by_RA = 0.75, Max_rate = 500, Min_rent = 100)

model_rent_assistance <- function(sample_file,
                                  .Prop_rent_paid_by_RA,
                                  Max_rate = NULL,
                                  Min_rent = NULL) {
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
  
  rent_assistance(fortnightly_rent = Rent, 
                  n_dependants = N_dependants,
                  has_partner = Has_partner,
                  is_homeowner = Is_homeowner,
                  lives_in_sharehouse = Lives_in_sharehouse,
                  .prop_rent_paid_by_RA = .Prop_rent_paid_by_RA,
                  max_rate = Max_rate,
                  min_rent = Min_rent)
}
