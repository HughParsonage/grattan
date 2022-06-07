#ifndef grattan_1991_H
#define grattan_1991_H
#include "grattan.h"


System System1991 = {
  .yr = 1991,
  .nb = 8,
  .BRACKETS = { 0, 5250, 17650, 20600, 20700, 35000, 36000, 50000 },
  .RATES = { 0, 0.205, 0.245, 0.295, 0.385, 0.425, 0.465, 0.47 },
  .M = {
    .lwr_single = 11746,
    .upr_single = 13819,
    .lwr_family = 19046,
    .upr_family = 22408,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1991,
    .upr_single_sapto = 2343,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.25,
    .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
    .year = 1991,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.205,
    .second_tax_rate = 0.245,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 17650,
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
