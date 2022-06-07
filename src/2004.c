#ifndef grattan_2004_H
#define grattan_2004_H
#include "grattan.h"


System System2004 = {
  .yr = 2004,
  .nb = 5,
  .BRACKETS = {0, 6000, 21600, 52000, 62500, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 15529,
    .upr_single = 16789,
    .lwr_family = 26205,
    .upr_family = 28331,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 20500,
    .upr_single_sapto = 22163,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 34303,
    .lwr_thr_up_per_child = 2406,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2004,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 20500,
    .lwr_couple = 33612,
    .upr_single = 38340,
    .upr_couple = 46428,
    .taper = 0.125,
    .first_tax_rate = 0.17,
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
