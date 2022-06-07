#ifndef grattan_2015_H
#define grattan_2015_H
#include "grattan.h"


System System2015 = {
  .yr = 2015,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 20896,
    .upr_single = 26120,
    .lwr_family = 35261,
    .upr_family = 44076,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 33044,
    .upr_single_sapto = 41305,
    .lwr_family_sapto = 46000,
    .upr_family_sapto = 57500,
    .lwr_thr_up_per_child = 3238,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2015,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 32279,
    .lwr_couple = 28974,
    .upr_single = 50119,
    .upr_couple = 41790,
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
  .has_temp_budget_repair_levy = true
};
#endif
