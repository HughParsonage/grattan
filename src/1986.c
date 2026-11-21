#ifndef grattan_1986_H
#define grattan_1986_H
#include "grattan.h"

System System1986 = {
  .yr = 1986,
  .nb = 6,
  .BRACKETS = { 0, 4595, 12500, 19500, 28000, 35000, INT_MAX, INT_MAX },
  .RATES = { 0, 0.25, 0.3, 0.46, 0.48, 0.6, 0.6, 0.6 },
  .M = {
    .lwr_single = 7526,
    .upr_single = 7923,
    .lwr_family = 12504,
    .upr_family = 13163,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 7526,
    .upr_single_sapto = 7923,
    .lwr_family_sapto = 12504,
    .upr_family_sapto = 13163,
    .lwr_thr_up_per_child = 1530,
    .taper = .20,
    .rate = .01
  },
  .has_sapto = false,
  .S = {
    .year = 1986,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.25,
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
