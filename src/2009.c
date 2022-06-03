#ifndef grattan_2009_H
#define grattan_2009_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2009 = 17794;
double ML_UPR_THRESHOLD_SINGLE_2009 = 20935;
double ML_LWR_THRESHOLD_FAMILY_2009 = 30025;
double ML_UPR_THRESHOLD_FAMILY_2009 = 35325;
double ML_LWR_THR_UP_PER_CHILD_2009 =  2757;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2009 = 28867;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2009 = 33962;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2009 = 42000;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2009 = 49413;
double ML_LWR_THRESHOLD_SINGLE_PTO_2009 = 25299;
double ML_LWR_THRESHOLD_FAMILY_PTO_2009 = 42000;
double ML_TAPER_2009 = 0.1;
double ML_RATE_2009 = 0.015;
double LITO_MAX_OFFSET_2009 = 1200;
double LITO_1ST_TAPER_2009 = -0.04;
double LITO_1ST_THRESH_2009 = 30000;
double SAPTO_MAX_SINGLE_2009 = 2230;
double SAPTO_LWR_SINGLE_2009 = 28867;
double SAPTO_UPR_SINGLE_2009 = 46707;
double SAPTO_MAX_MARRIED_2009 = 2230;
double SAPTO_LWR_MARRIED_2009 = 28867;
double SAPTO_UPR_MARRIED_2009 = 46707;
double SAPTO_TAPER_2009 = -0.125;

System System2009 = {
  .yr = 2009,
  .nb = 5,
  .BRACKETS = {0, 6000, 34000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.4, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 17794,
    .upr_single = 20935,
    .lwr_family = 30025,
    .upr_family = 35324,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 28867,
    .upr_single_sapto = 33962,
    .lwr_family_sapto = 42000,
    .upr_family_sapto = 49412,
    .lwr_thr_up_per_child = 2757,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2009,
    .pension_age = 65,
    .mxo_single = 2230,
    .mxo_couple = 2230,
    .lwr_single = 28867,
    .lwr_couple = 28867,
    .upr_single = 46707,
    .upr_couple = 46707,
    .taper = 0.125,
    .first_tax_rate = 0.15,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 34000,
    .lito_max_offset = 1200,
    .lito_1st_thresh = 30000,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 1200,
      .Thresholds = 30000,
      .Tapers = -0.04,
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
