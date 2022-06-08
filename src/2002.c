#ifndef grattan_2002_H
#define grattan_2002_H
#include "grattan.h"


System System2002 = {
  .yr = 2002,
  .nb = 5,
  .BRACKETS = {0, 6000, 20000, 50000, 60000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 14540,
    .upr_single = 17106,
    .lwr_family = 24534,
    .upr_family = 28864,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 20000,
    .upr_single_sapto = 23530,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 37329,
    .lwr_thr_up_per_child = 2253,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2002,
    .pension_age = 65,
    .mxo_single = 1710,
    .mxo_couple = 2490,
    .lwr_single = 16059,
    .lwr_couple = 26648,
    .upr_single = 29739,
    .upr_couple = 46568,
    .taper = 0.125,
    .first_tax_rate = 0.17,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 20000,
    .lito_max_offset = 150,
    .lito_1st_thresh = 20700,
    .lito_1st_taper = 0.04
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
