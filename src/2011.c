#ifndef grattan_2011_H
#define grattan_2011_H
#include "grattan.h"


System System2011 = {
  .yr = 2011,
  .nb = 5,
  .BRACKETS = {0, 6000, 37000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.30, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 18839,
    .upr_single = 22164,
    .lwr_family = 31789,
    .upr_family = 37399,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 30685,
    .upr_single_sapto = 36100,
    .lwr_family_sapto = 44500,
    .upr_family_sapto = 52353,
    .lwr_thr_up_per_child = 2919,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2011,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 30685,
    .lwr_couple = 26680,
    .upr_single = 48525,
    .upr_couple = 78992,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 37000,
    .lito_max_offset = 1500,
    .lito_1st_thresh = 30000,
    .lito_1st_taper = 0.04,
    .mxo_illness = 2040,
    .lwr_illness = 29600,
    .upr_illness = 91840
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 1500,
      .Thresholds = {30000},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
