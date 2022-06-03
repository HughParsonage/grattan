#ifndef grattan_2008_H
#define grattan_2008_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2008 = 17309;
double ML_UPR_THRESHOLD_SINGLE_2008 = 20365;
double ML_LWR_THRESHOLD_FAMILY_2008 = 29207;
double ML_UPR_THRESHOLD_FAMILY_2008 = 34362;
double ML_LWR_THR_UP_PER_CHILD_2008 =  2682;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2008 = 25867;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2008 = 30433;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2008 = 33500;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2008 = 39413;
double ML_LWR_THRESHOLD_SINGLE_PTO_2008 = 22922;
double ML_LWR_THRESHOLD_FAMILY_PTO_2008 = 33500;
double ML_TAPER_2008 = 0.1;
double ML_RATE_2008 = 0.015;
double LITO_MAX_OFFSET_2008 = 750;
double LITO_1ST_TAPER_2008 = -0.04;
double LITO_1ST_THRESH_2008 = 30000;
double SAPTO_MAX_MARRIED_2008 = 3204;
double SAPTO_LWR_MARRIED_2008 = 41360;
double SAPTO_UPR_MARRIED_2008 = 66992;
double SAPTO_MAX_SINGLE_2008 = 2230;
double SAPTO_LWR_SINGLE_2008 = 24867;
double SAPTO_UPR_SINGLE_2008 = 42707;
double SAPTO_TAPER_2008 = -0.125;
System System2008 = {
    .yr = 2008,
  .nb = 5,
  .BRACKETS = {0, 6000, 30000, 75000, 150000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.4, 0.45, 0.45, 0.45, 0.45},
  .M = {
  .lwr_single = 17309,
  .upr_single = 20364,
  .lwr_family = 29207,
  .upr_family = 34362,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 25867,
  .upr_single_sapto = 30432,
  .lwr_family_sapto = 33500,
  .upr_family_sapto = 39412,
  .lwr_thr_up_per_child = 2682,
  .taper = 0.1,
  .rate = 0.015
  },
  .has_sapto = true,
  .S = {
  .year = 2008,
  .pension_age = 65,
  .mxo_single = 2230,
  .mxo_couple = 3204,
  .lwr_single = 24867,
  .lwr_couple = 41360,
  .upr_single = 42707,
  .upr_couple = 66992,
  .taper = 0.125,
  .first_tax_rate = 0.15,
  .second_tax_rate = 0.3,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 30000,
  .lito_max_offset = 750,
  .lito_1st_thresh = 30000,
  .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
.Offsets = {
{
  // LITO
  .offset_1st = 750,
  .Thresholds = 30000,
  .Tapers = -0.04,
  .nb = 1,
  .refundable = false
}
},
  .has_temp_budget_repair_levy = false
};
#endif
