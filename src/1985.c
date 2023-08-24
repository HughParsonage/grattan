#ifndef grattan_1985_H
#define grattan_1985_H
#include "grattan.h"

System System1985 = {
  .yr = 1985,
  .nb = 7,
  .BRACKETS = { 0, 4595, 12500, 19500, 28000, 35000, 35788, INT_MAX },
  .RATES = { 0, 0.2667, 0.3, 0.46, 0.4733, 0.5533, 0.6, 0.6 },
  .M = {
    .lwr_single = 7111,
    .upr_single = 7485, //8366,
    .lwr_family = 11804,
    .upr_family = 12426, //13888,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1985,
    .upr_single_sapto = 2336,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 1330,
    .taper = 0.25,
    .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
    .year = 1985,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2667,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 12500,
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
