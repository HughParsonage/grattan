#ifndef grattan_2000_H
#define grattan_2000_H
#include "grattan.h"
System System2000 = {
  .yr = 2000,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 13550,
    .upr_single = 14649,
    .lwr_family = 22865,
    .upr_family = 24719,
    .has_sapto_thr = false,
    .sapto_age = 65,
    .lwr_single_sapto = 13551,
    .upr_single_sapto = 14650,
    .lwr_family_sapto = 22866,
    .upr_family_sapto = 24720,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2000,
    .pension_age = 65,
    .mxo_single = 1358,
    .mxo_couple = 980,
    .lwr_single = 12190,
    .lwr_couple = 10300,
    .upr_single = 23054,
    .upr_couple = 36280,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.34,
    .tax_free_thresh = 5400,
    .tax_2nd_thresh = 20700,
    .lito_max_offset = 0,
    .lito_1st_thresh = 0,
    .lito_1st_taper = 0,
    .mxo_illness = 1296,
    .lwr_illness = 11880,
    .upr_illness = 44496
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
