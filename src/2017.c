#ifndef grattan_2017_H
#define grattan_2017_H
#include "grattan.h"


System System2017 = {
  .yr = 2017,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 87000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 21665,
    .upr_single = 27081,
    .lwr_family = 36541,
    .upr_family = 45676,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 34244,
    .upr_single_sapto = 42805,
    .lwr_family_sapto = 47670,
    .upr_family_sapto = 59588,
    .lwr_thr_up_per_child = 3356,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2017,
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
