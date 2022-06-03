#ifndef grattan_2006_H
#define grattan_2006_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2006 = 16284;
double ML_UPR_THRESHOLD_SINGLE_2006 = 17605;
double ML_LWR_THRESHOLD_FAMILY_2006 = 27478;
double ML_UPR_THRESHOLD_FAMILY_2006 = 29707;
double ML_LWR_THR_UP_PER_CHILD_2006 =  2523;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2006 = 21968;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2006 = 23750;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2006 = 31729;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2006 = 34303;
double ML_LWR_THRESHOLD_SINGLE_PTO_2006 = 19583;
double ML_LWR_THRESHOLD_FAMILY_PTO_2006 = 31729;
double ML_TAPER_2006 = 0.1;
double ML_RATE_2006 = 0.015;
double LITO_MAX_OFFSET_2006 = 235;
double LITO_1ST_TAPER_2006 = -0.04;
double LITO_1ST_THRESH_2006 = 21600;
double SAPTO_MAX_MARRIED_2006 = 3204;
double SAPTO_LWR_MARRIED_2006 = 36494;
double SAPTO_UPR_MARRIED_2006 = 31063;
double SAPTO_MAX_SINGLE_2006 = 2230;
double SAPTO_LWR_SINGLE_2006 = 21968;
double SAPTO_UPR_SINGLE_2006 = 39808;
double SAPTO_TAPER_2006 = -0.125;
System System2006 = {
  .yr = 2006,
  .nb = 5 ,
  .BRACKETS = {0, 6000, 21600, 63000, 95000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 16284,
    .upr_single = 19158,
    .lwr_family = 27478,
    .upr_family = 32328,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 21968,
    .upr_single_sapto = 25845,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 37329,
    .lwr_thr_up_per_child = 2523,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2006,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 3204,
    .lwr_single = 21968,
    .lwr_couple = 36494,
    .upr_single = 39808,
    .upr_couple = 62126,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 21600,
    .lito_max_offset = 235,
    .lito_1st_thresh = 21600,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 235,
      .Thresholds = {21600},
      .Tapers = {-0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
