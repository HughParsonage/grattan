#ifndef grattan_1993_H
#define grattan_1993_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_1993 = 11888;
double ML_UPR_THRESHOLD_SINGLE_1993 = 12680;
double ML_LWR_THRESHOLD_FAMILY_1993 = 20071;
double ML_UPR_THRESHOLD_FAMILY_1993 = 21408;
double ML_LWR_THR_UP_PER_CHILD_1993 =  2100;
double ML_TAPER_1993 = 0.25;
double ML_RATE_1993 = 0.0125;
System System1993 = {
  .yr = 1993,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 36000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.38, 0.46, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 11888,
    .upr_single = 13986,
    .lwr_family = 20071,
    .upr_family = 23613,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1993,
    .upr_single_sapto = 2345,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.25,
    .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
    .year = 1993,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.38,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 20700,
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
