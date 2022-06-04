#ifndef grattan_2012_H
#define grattan_2012_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2012 = 19404;
double ML_UPR_THRESHOLD_SINGLE_2012 = 22829;
double ML_LWR_THRESHOLD_FAMILY_2012 = 32743;
double ML_UPR_THRESHOLD_FAMILY_2012 = 38522;
double ML_LWR_THR_UP_PER_CHILD_2012 =  3007;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2012 = 30685;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2012 = 36101;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2012 = 44500;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2012 = 52354;
double ML_TAPER_2012 = 0.1;
double ML_RATE_2012 = 0.015;
double LITO_MAX_OFFSET_2012 = 1500;
double LITO_1ST_TAPER_2012 = -0.04;
double LITO_1ST_THRESH_2012 = 30000;
double SAPTO_MAX_SINGLE_2012 = 2230;
double SAPTO_LWR_SINGLE_2012 = 30685;
double SAPTO_UPR_SINGLE_2012 = 48525;
double SAPTO_MAX_MARRIED_2012 = 2230;
double SAPTO_LWR_MARRIED_2012 = 30685;
double SAPTO_UPR_MARRIED_2012 = 48525;
double SAPTO_TAPER_2012 = -0.125;


System System2012 = {
  .yr = 2012,
  .nb = 5,
  .BRACKETS = {0, 6000, 37000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.30, 0.37, 0.45, 0.45, 0.45, 0.45},
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
    .upr_family_sapto = 52352,
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
