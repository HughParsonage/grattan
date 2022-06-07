#ifndef grattan_2009_H
#define grattan_2009_H
#include "grattan.h"


System System2009 = {
  .yr = 2009,
  .nb = 5,
  .BRACKETS = {0, 6000, 34000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.4, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 17794,
    .upr_single = 20935,
    .lwr_family = 30025,
    .upr_family = 35324,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 28867,
    .upr_single_sapto = 33962,
    .lwr_family_sapto = 42000,
    .upr_family_sapto = 49412,
    .lwr_thr_up_per_child = 2757,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2009,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 2230,
    .lwr_single = 28867,
    .lwr_couple = 28867,
    .upr_single = 46707,
    .upr_couple = 46707,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 34000,
    .lito_max_offset = 1200,
    .lito_1st_thresh = 30000,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 1200,
      .Thresholds = {30000},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
