#ifndef grattan_1990_H
#define grattan_1990_H
#include "grattan.h"

System System1990 = {
  .yr = 1990,
  .nb = 6,
  .BRACKETS = { 0, 5100, 17650, 20600, 35000, 50000, INT_MAX, INT_MAX },
  .RATES = { 0, 0.21, 0.29, 0.39, 0.47, 0.48, 0.48, 0.48 },
  .M = {
    .lwr_single = 10331,
    .upr_single = 12155,
    .lwr_family = 17401,
    .upr_family = 20472,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1990,
    .upr_single_sapto = 2342,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.25,
    .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
    .year = 1990,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.21,
    .second_tax_rate = 0.29,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 17650,
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
