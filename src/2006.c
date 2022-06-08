#ifndef grattan_2006_H
#define grattan_2006_H
#include "grattan.h"

System System2006 = {
  .yr = 2006,
  .nb = 5 ,
  .BRACKETS = {0, 6000, 21600, 63000, 95000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 16284,
    .upr_single = 19158,
    .lwr_family = 27478,
    .upr_family = 32328,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 21968,
    .upr_single_sapto = 25845,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 37329,
    .lwr_thr_up_per_child = 2523,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2006,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 3204,
    .lwr_single = 21968,
    .lwr_couple = 36494,
    .upr_single = 39808,
    .upr_couple = 62126,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 21600,
    .lito_max_offset = 235,
    .lito_1st_thresh = 21600,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 235,
      .Thresholds = {21600},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
