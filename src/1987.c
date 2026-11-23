#ifndef grattan_1987_H
#define grattan_1987_H
#include "grattan.h"

System System1987 = {
  .yr = 1987,
  .nb = 7,
  .BRACKETS = { 0, 4890, 12500, 12600, 19500, 28000, 35000, INT_MAX },
  .RATES = { 0, 0.2442, 0.265, 0.2942, 0.4425, 0.4683, 0.5708, 0.5708 },
  .M = {
    .lwr_single = 8030,
    .upr_single = 8518,
    .lwr_family = 13370,
    .upr_family = 14182,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 8030,
    .upr_single_sapto = 8518,
    .lwr_family_sapto = 13370,
    .upr_family_sapto = 14182,
    .lwr_thr_up_per_child = 1660,
    .taper = .20,
    .rate = .01145
  },
  .has_sapto = false,
  .S = {
    .year = 1987,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2442,
    .second_tax_rate = 0.265,
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
