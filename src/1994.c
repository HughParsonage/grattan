#ifndef grattan_1994_H
#define grattan_1994_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_1994 = 12689;
double ML_UPR_THRESHOLD_SINGLE_1994 = 13643;
double ML_LWR_THRESHOLD_FAMILY_1994 = 21367;
double ML_UPR_THRESHOLD_FAMILY_1994 = 22974;
double ML_LWR_THR_UP_PER_CHILD_1994 =  2100;
double ML_TAPER_1994 = 0.20;
double ML_RATE_1994 = 0.014;
double LITO_MAX_OFFSET_1994 = 150;
double LITO_1ST_TAPER_1994 = -0.04;
double LITO_1ST_THRESH_1994 = 20700;
System System1994 = {
  .yr = 1994,
  .nb = 6,
  .BRACKETS = { 0, 5400, 20700, 36000, 38000, 50000, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.355, 0.385, 0.44125, 0.47, 0.47, 0.47 },
  .M = {
    .lwr_single = 12689,
    .upr_single = 14929,
    .lwr_family = 21367,
    .upr_family = 25138,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 1994,
    .upr_single_sapto = 2346,
    .lwr_family_sapto = 16843008,
    .upr_family_sapto = 19815304,
    .lwr_thr_up_per_child = 2100,
    .taper = 0.2,
    .rate = 0.014
  },
  .has_sapto = false,
  .S = {
    .year = 1994,
    .pension_age = 65,
    .mxo_single = 0,
    .mxo_couple = 0,
    .lwr_single = 0,
    .lwr_couple = 0,
    .upr_single = 0,
    .upr_couple = 0,
    .taper = 0.125,
    .first_tax_rate = 0.2,
    .second_tax_rate = 0.355,
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
      .Thresholds = 20700,
      .Tapers = -0.04,
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
