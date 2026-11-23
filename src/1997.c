#ifndef grattan_1997_H
#define grattan_1997_H
#include "grattan.h"

System System1997 = {
  .yr = 1997,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 13127,
    .upr_single = 14347,
    .lwr_family = 22152,
    .upr_family = 24210,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 13127,
    .upr_single_sapto = 14347,
    .lwr_family_sapto = 22152,
    .upr_family_sapto = 24210,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.2,
    .rate = 0.017
  },
  .has_sapto = true,
  .S = {
    .year = 1997,
    .pension_age = 65,
    .mxo_single = 1229,
    .mxo_couple = 868,
    .lwr_single = 11545,
    .lwr_couple = 9740,
    .upr_single = 21377,
    .upr_couple = 33368,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.34,
    .tax_free_thresh = 5400,
    .tax_2nd_thresh = 20700,
    .lito_max_offset = 0,
    .lito_1st_thresh = 0,
    .lito_1st_taper = 0,
    .mxo_illness = 1166,
    .lwr_illness = 11230,
    .upr_illness = 41116
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 150,
      .Thresholds = {20700},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
