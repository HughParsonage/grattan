#ifndef grattan_2004_H
#define grattan_2004_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2004 = 15529;
double ML_UPR_THRESHOLD_SINGLE_2004 = 16789;
double ML_LWR_THRESHOLD_FAMILY_2004 = 26205;
double ML_UPR_THRESHOLD_FAMILY_2004 = 28331;
double ML_LWR_THR_UP_PER_CHILD_2004 =  2406;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2004 = 20500;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2004 = 22163;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2004 = 31729;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2004 = 34303;
double ML_LWR_THRESHOLD_SINGLE_PTO_2004 = 18141;
double ML_LWR_THRESHOLD_FAMILY_PTO_2004 = 31729;
double ML_TAPER_2004 = 0.20;
double ML_RATE_2004 = 0.015;
double LITO_MAX_OFFSET_2004 = 235;
double LITO_1ST_TAPER_2004 = -0.04;
double LITO_1ST_THRESH_2004 = 21600;
double SAPTO_MAX_SINGLE_2004 = 2230;
double SAPTO_MAX_MARRIED_2004 = 1602;
double SAPTO_LWR_SINGLE_2004 = 20500;
double SAPTO_LWR_MARRIED_2004 = 33612;
double PTO_MAX_SINGLE_2004 = 1925;
double PTO_MAX_MARRIED_2004 = 2848;
double PTO_LWR_SINGLE_2004 = 17342;
double PTO_LWR_MARRIED_2004 = 28754;
double SAPTO_TAPER_2004 = -0.125;

System System2004 = {
  .yr = 2004,
  .nb = 5,
  .BRACKETS = {0, 6000, 21600, 52000, 62500, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 15529,
    .upr_single = 16789,
    .lwr_family = 26205,
    .upr_family = 28331,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 20500,
    .upr_single_sapto = 22163,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 34303,
    .lwr_thr_up_per_child = 2406,
    .taper = 0.2,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2004,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .lwr_single = 20500,
    .lwr_couple = 33612,
    .upr_single = 38340,
    .upr_couple = 46428,
    .taper = 0.125,
    .first_tax_rate = 0.17,
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
      .Tapers = {0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
