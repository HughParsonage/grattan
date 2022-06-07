#ifndef grattan_2007_H
#define grattan_2007_H
#include "grattan.h"

System System2007 = {
  .yr = 2007,
  .nb = 5,
  .BRACKETS = {0, 6000, 25000, 75000, 150000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.4, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 16740,
    .upr_single = 19695,
    .lwr_family = 28247,
    .upr_family = 33232,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 24867,
    .upr_single_sapto = 29256,
    .lwr_family_sapto = 33500,
    .upr_family_sapto = 39412,
    .lwr_thr_up_per_child = 2594,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2007,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 3204,
    .lwr_single = 24867,
    .lwr_couple = 43360,
    .upr_single = 42707,
    .upr_couple = 68992,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 25000,
    .lito_max_offset = 600,
    .lito_1st_thresh = 25000,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 600,
      .Thresholds = {25000},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
