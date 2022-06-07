#ifndef grattan_1992_H
#define grattan_1992_H
#include "grattan.h"

System System1992 = {
  .yr = 1992,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 36000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.38, 0.46, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 11746,
    .upr_single = 13819,
    .lwr_family = 19675,
    .upr_family = 23148,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1992,
    .upr_single_sapto = 2344,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.25,
    .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
    .year = 1992,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.38,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 20700,
    .lito_max_offset = 0,
    .lito_1st_thresh = 0,
    .lito_1st_taper = 0
  },
  .n_offsetn = 0,
  .Offsets = {
    {
      // DUMMY
      .offset_1st = 0,
      .Thresholds = {0},
      .Tapers = {0},
      .nb = 0,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
