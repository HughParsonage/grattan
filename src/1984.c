#ifndef grattan_1984_H
#define grattan_1984_H
#include "grattan.h"

System System1984 = {
  .yr = 1984,
  .nb = 4,
  .BRACKETS = { 0, 4595, 19500, 35788, INT_MAX, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.3, 0.46, 0.6, 0.6, 0.6, 0.6, 0.6 },
  // Upper thresholds are the first integer incomes at the full levy rate,
  // rather than inclusive statutory phase-in limits (last reduced income).
  .M = {
    .lwr_single = 6698,
    .upr_single = 7051,
    .lwr_family = 11141,
    .upr_family = 11728,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 6698,
    .upr_single_sapto = 7051,
    .lwr_family_sapto = 11141,
    .upr_family_sapto = 11728,
    .lwr_thr_up_per_child = 1100,
    .taper = .0832,
    .rate = .00416,
    .max_levy = 291.20
  },
  .has_sapto = false,
  .S = {
    .year = 1984,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.3,
    .second_tax_rate = 0.46,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 19500,
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
