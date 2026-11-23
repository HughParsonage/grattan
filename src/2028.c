#ifndef grattan_2028_H
#define grattan_2028_H
#include "grattan.h"

// Income Tax Rates Act 1986 Sch 7 and Medicare Levy Act 1986,
// as enacted at 1 July 2026; later Medicare indexation requires a law update.
System System2028 = {
  .yr = 2028,
  .nb = 5,
  .BRACKETS = {0, 18200, 45000, 135000, 190000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, .14, 0.300, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 28011,
    .upr_single = 35014,
    .lwr_family = 47238,
    .upr_family = 59048,
    .has_sapto_thr = true,
    .sapto_age = 67,
    .lwr_single_sapto = 44268,
    .upr_single_sapto = 55335,
    .lwr_family_sapto = 61623,
    .upr_family_sapto = 77029,
    .lwr_thr_up_per_child = 4338,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2028,
    .pension_age = 67,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .mxo_illness = 2040,
    .lwr_single = 37137,
    .lwr_couple = 32822,
    .lwr_illness = 35950,
    .upr_single = 54977,
    .upr_couple = 91276,
    .upr_illness = 104540,
    .taper = 0.125,
    .first_tax_rate = .14,
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
