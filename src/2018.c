#ifndef grattan_2018_H
#define grattan_2018_H
#include "grattan.h"


System System2018 = {
  .yr = 2018,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 87000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M =
  {
    .lwr_single = 21980,
    .upr_single = 27476,
    .lwr_family = 37089,
    .upr_family = 46363,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 34758,
    .upr_single_sapto = 43449,
    .lwr_family_sapto = 48385,
    .upr_family_sapto = 60483,
    .lwr_thr_up_per_child = 3406,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S =
  {
    .year = 2018,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .mxo_illness = 2040,
    .lwr_single = 32279,
    .lwr_couple = 28974,
    .lwr_illness = 31279,
    .upr_single = 50119,
    .upr_couple = 83580,
    .upr_illness = 95198,
    .taper = 0.125,
    .first_tax_rate = 0.19,
    .second_tax_rate = 0.325,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 37000,
    .lito_max_offset = 445,
    .lito_1st_thresh = 37000,
    .lito_1st_taper = 0.015
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 445,
      .Thresholds = {37000},
      .Tapers = {0.015},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};

#endif
