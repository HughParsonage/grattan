#ifndef grattan_2022_H
#define grattan_2022_H
#include "grattan.h"

System System2022 = {
  .yr = 2022,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 90000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 22801,
    .upr_single = 28501,
    .lwr_family = 38474,
    .upr_family = 48092,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 36056,
    .upr_single_sapto = 45071,
    .lwr_family_sapto = 50191,
    .upr_family_sapto = 62738,
    .lwr_thr_up_per_child = 3533,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2022,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 33622,
    .lwr_couple = 30316,
    .upr_single = 51462,
    .upr_couple = 43132,
    .taper = 0.125,
    .first_tax_rate = 0.19,
    .second_tax_rate = 0.325,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 37000,
    .lito_max_offset = 445,
    .lito_1st_thresh = 37000,
    .lito_1st_taper = 0.015
  },
  .n_offsetn = 2,
  .Offsets = {
    {
      // LITO
      .offset_1st = 700,
      .Thresholds = {37500, 45000},
      .Tapers = {0.05, 0.015},
      .nb = 2,
      .refundable = false
    },
    {
      // LMITO
      .offset_1st = 255,
      .Thresholds = {37000, 48000, 90000},
      .Tapers = {-0.075, 0, 0.03},
      .nb = 3,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
