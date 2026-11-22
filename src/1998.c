#ifndef grattan_1998_H
#define grattan_1998_H
#include "grattan.h"

System System1998 = {
  .yr = 1998,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 13389,
    .upr_single = 14475,
    .lwr_family = 22594,
    .upr_family = 24426,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 13389,
    .upr_single_sapto = 14475,
    .lwr_family_sapto = 22594,
    .upr_family_sapto = 24426,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 1998,
    .pension_age = 65,
    .mxo_single = 1260,
    .mxo_couple = 896,
    .lwr_single = 11700,
    .lwr_couple = 9880,
    .upr_single = 21780,
    .upr_couple = 34096,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.34,
    .tax_free_thresh = 5400,
    .tax_2nd_thresh = 20700,
    .lito_max_offset = 0,
    .lito_1st_thresh = 0,
    .lito_1st_taper = 0,
    .mxo_illness = 1197,
    .lwr_illness = 11385,
    .upr_illness = 41922
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 150,
      .Thresholds = {20700},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
