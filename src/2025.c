#ifndef grattan_2025_H
#define grattan_2025_H
#include "grattan.h"

System System2025 = {
  .yr = 2025,
  .nb = 5,
  .BRACKETS = {0, 18200, 45000, 135000, 190000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.16, 0.300, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 27222,
    .upr_single = 34028,
    .lwr_family = 45907,
    .upr_family = 57384,
    .has_sapto_thr = true,
    .sapto_age = 67,
    .lwr_single_sapto = 43020,
    .upr_single_sapto = 53775,
    .lwr_family_sapto = 59886,
    .upr_family_sapto = 74858,
    .lwr_thr_up_per_child = 4216,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2025,
    .pension_age = 67,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .mxo_illness = 2040,
    .lwr_single = 34919,
    .lwr_couple = 30994,
    .lwr_illness = 33732,
    .upr_single = 52759,
    .upr_couple = 87620,
    .upr_illness = 100104,
    .taper = 0.125,
    .first_tax_rate = 0.16,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 18200,
    .tax_2nd_thresh = 45000,
    .lito_max_offset = 445,
    .lito_1st_thresh = 37000,
    .lito_1st_taper = 0.015
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 700,
      .Thresholds = {37500, 45000},
      .Tapers = {0.05, 0.015},
      .nb = 2,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
