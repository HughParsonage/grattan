#ifndef grattan_1997_H
#define grattan_1997_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_1997 = 13128;
double ML_UPR_THRESHOLD_SINGLE_1997 = 14346;
double ML_LWR_THRESHOLD_FAMILY_1997 = 22153;
double ML_UPR_THRESHOLD_FAMILY_1997 = 24209;
double ML_LWR_THR_UP_PER_CHILD_1997 =  2100;
double ML_TAPER_1997 = 0.20;
double ML_RATE_1997 = 0.017;
double LITO_MAX_OFFSET_1997 = 150;
double LITO_1ST_TAPER_1997 = -0.04;
double LITO_1ST_THRESH_1997 = 20700;
System System1997 = {
  .yr = 1997,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 13128,
    .upr_single = 15445,
    .lwr_family = 22153,
    .upr_family = 26063,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1997,
    .upr_single_sapto = 2350,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.2,
    .rate = 0.017
  },
  .has_sapto = false,
  .S = {
    .year = 1997,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.34,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 20700,
    .lito_max_offset = 0,
    .lito_1st_thresh = 0,
    .lito_1st_taper = 0
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 150,
      .Thresholds = {20700},
      .Tapers = {-0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
