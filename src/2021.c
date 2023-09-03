#ifndef grattan_2021_H
#define grattan_2021_H

#include "grattan.h"



System System2021 = {
  .yr = 2021,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 90000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M =
  {
    .lwr_single = 23226,
    .upr_single = 29033,
    .lwr_family = 39167,
    .upr_family = 48959,
    .has_sapto_thr = true,
    .sapto_age = 66,
    .lwr_single_sapto = 36056,
    .upr_single_sapto = 45071,
    .lwr_family_sapto = 51094,
    .upr_family_sapto = 63868,
    .lwr_thr_up_per_child = 3597,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2021,
    .pension_age = 66.5,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .mxo_illness = 2040,
    .lwr_single = 32279,
    .lwr_couple = 28974,
    .lwr_illness = 31279,
    .upr_single = 50119,
    .upr_couple = 83580,
    .upr_illness = 95198,
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
