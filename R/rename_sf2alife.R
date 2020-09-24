#' Rename sample files to alife variables
#' 
#' @noRd
#' @name rename_sf2alife
#' 
#' @param sample_file A data.table
#' @param invert Rename from alife to sample file


rename_sf2alife <- function(sample_file) {
  lookup <-
    c("ic_taxable_income_loss" = "Taxable_Income",
      "is_net_rent" = "Net_rent_amt", 
      "")
}