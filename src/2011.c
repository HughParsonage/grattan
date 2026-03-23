#ifndef grattan_2011_H
#define grattan_2011_H
#include "grattan.h"


System System2011 = {
  .yr = 2011,
  .nb = 7,
  .BRACKETS = {0, 6000, 37000, 50000, 80000, 100000, 180000, INT_MAX}, // includes flood levy
  .RATES =    {0, 0.15, 0.300, 0.305, 0.375, 0.3800, 0.4600, 0.46000},
  .M = {
    .lwr_single = 19404,
    .upr_single = 22829,
    .lwr_family = 32743,
    .upr_family = 38522,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 30685,
    .upr_single_sapto = 36101,
    .lwr_family_sapto = 44500,
    .upr_family_sapto = 52354,
    .lwr_thr_up_per_child = 3007,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2014,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 30685,
    .lwr_couple = 28974,
    .upr_single = 48525,
    .upr_couple = 41790,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.30,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 37000,
    .lito_max_offset = 1500,
    .lito_1st_thresh = 30000,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 1500,
      .Thresholds = {30000},
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
