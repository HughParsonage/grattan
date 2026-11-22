# ITAA 1997 ss 61-155, 61-160 and 25-130. Historical return items
# support projections. The complete supplied statutory amount takes priority.
net_labour_from_return <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }
  columns <- names(data)
  # Each argument describes one amount and its aliases. Absent items do not
  # need a zero vector or an addition. A double accumulator avoids overflow.
  sum_fields <- function(...) {
    total <- NULL
    for (aliases in list(...)) {
      if (!any(aliases %chin% columns)) {
        next
      }
      x <- return_field(data, aliases, default = 0)
      total <-
        if (is.null(total)) {
          as.double(x)
        } else {
          total + x
        }
    }
    total
  }
  net <- return_field(data, "net_labour_income", default = {
    wages <- sum_fields(c("i_salary_wage", "Sw_amt"), c("i_allowances", "Alow_ben_amt"),
                        c("i_etp", "ETP_txbl_amt"), "i_lsum_a", "i_lsum_b") %||% 0
    expenses <- sum_fields(c("d_wre_car", "WRE_car_amt"), c("d_wre_travel", "WRE_trvl_amt"),
                           c("d_wre_clothing", "WRE_uniform_amt"), c("d_wre_self_edu", "WRE_self_amt"),
                           c("d_wre_other", "WRE_other_amt"))
    deduction <- hutilscpp::squish(wages, 0, 1000)
    if (!is.null(expenses)) {
      deduction <-
        if (length(deduction) == length(expenses)) {
          pmaxV(expenses, deduction)
        } else {
          pmaxC(expenses, deduction)
        }
    }
    amount <- wages - deduction
    sole_trade <- sum_fields(c("is_bus_pp", "Net_PP_BI_amt"), c("is_bus_npp", "Net_NPP_BI_amt"))
    if (!is.null(sole_trade)) {
      amount <- amount + sole_trade
    }
    psi <- sum_fields("i_attributed_psi", "is_psi_net")
    if (!is.null(psi)) {
      amount <- amount + psi
    }
    # Total assessable discounts exclude the upfront reduction. Remove the
    # deferred and pre-2009 amounts when the return identifies them.
    if (all(c("i_ess", "in_ess_disc_defer") %chin% columns)) {
      upfront <- as.double(data$i_ess) - data$in_ess_disc_defer
      if ("in_ess_interest_pre2009" %chin% columns) {
        upfront <- upfront - return_field(data, "in_ess_interest_pre2009", default = 0)
      }
      upfront <- pmax0(upfront)
      if (anyNA(upfront)) {
        upfront <- fcoalesce(upfront, as.double(return_field(data, "in_ess_disc_inelgbl", default = 0)))
      }
      amount <- amount + upfront
    } else if ("in_ess_disc_inelgbl" %chin% columns) {
      amount <- amount + return_field(data, "in_ess_disc_inelgbl", default = 0)
    }
    pmax0(amount, in_place = TRUE)
  })
  resident <- data[["c_resident"]]
  if (is.null(resident)) {
    return(net)
  }
  nonresident <- if (is.numeric(resident) || is.logical(resident)) {
    flag <- resident == 0
    if (anyNA(flag)) {
      coalesce0(flag)
    } else {
      flag
    }
  } else {
    as.character(resident) %chin% c("N", "0", "FALSE")
  }
  if (length(net) == length(nonresident) && !any(nonresident)) {
    return(net)
  }
  fifelse(nonresident, 0, net)
}
