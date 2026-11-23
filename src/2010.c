#ifndef grattan_2010_H
#define grattan_2010_H
#include "grattan.h"


System System2010 = {
  .yr = 2010,
  .nb = 5,
  .BRACKETS = {0, 6000, 35000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.30, 0.38, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 18488,
    .upr_single = 21751,
    .lwr_family = 31196,
    .upr_family = 36702,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 29867,
    .upr_single_sapto = 35138,
    .lwr_family_sapto = 43500,
    .upr_family_sapto = 51177,
    .lwr_thr_up_per_child = 2865,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2010,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 29867,
    .lwr_couple = 25680,
    .upr_single = 47707,
    .upr_couple = 76992,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 35000,
    .lito_max_offset = 1350,
    .lito_1st_thresh = 30000,
    .lito_1st_taper = 0.04,
    .mxo_illness = 2040,
    .lwr_illness = 28600,
    .upr_illness = 89840
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 1350,
      .Thresholds = {30000},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
