# ITAA 1997 ss 61-155, 61-160 and 25-130. Historical return items
# support projections. The complete supplied statutory amount takes priority.
net_labour_from_return <- function(data) {
  if (is.null(data)) return(NULL)
  field <- function(...) as.double(return_field(data, ..., default = 0))
  wages <- field("i_salary_wage", "Sw_amt") + field("i_allowances", "Alow_ben_amt") +
    field("i_etp", "ETP_txbl_amt") + field("i_lsum_a") + field("i_lsum_b")
  work_expenses <- field("d_wre_car", "WRE_car_amt") + field("d_wre_travel", "WRE_trvl_amt") +
    field("d_wre_clothing", "WRE_uniform_amt") + field("d_wre_self_edu", "WRE_self_amt") +
    field("d_wre_other", "WRE_other_amt")
  sole_trade <- field("is_bus_pp", "Net_PP_BI_amt") + field("is_bus_npp", "Net_NPP_BI_amt")
  psi <- field("i_attributed_psi") + field("is_psi_net")
  # Total assessable ESS discounts are already net of the upfront reduction.
  # Remove deferred and pre-2009 amounts where the return supplies the total.
  upfront <- field("in_ess_disc_inelgbl")
  if (all(c("i_ess", "in_ess_disc_defer") %in% names(data))) {
    upfront <- return_field(data, "i_ess", default = NA_real_) -
      data$in_ess_disc_defer - field("in_ess_interest_pre2009")
    upfront <- ifelse(is.na(upfront), field("in_ess_disc_inelgbl"), pmax(0, upfront))
  }
  standard <- pmin(1000, pmax(wages, 0))
  net <- wages - pmax(work_expenses, standard) + sole_trade + psi + upfront
  net <- return_field(data, "net_labour_income", default = pmax(0, net))
  resident <- data[["c_resident"]]
  if (is.null(resident)) net else ifelse(resident %in% c("N", 0, FALSE), 0, net)
}
