#ifndef grattan_2001_H
#define grattan_2001_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2001 = 13808;
double ML_UPR_THRESHOLD_SINGLE_2001 = 14927;
double ML_LWR_THRESHOLD_FAMILY_2001 = 23300;
double ML_UPR_THRESHOLD_FAMILY_2001 = 25190;
double ML_LWR_THR_UP_PER_CHILD_2001 =  2140;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2001 = 20000;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2001 = 21623;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2001 = 31729;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2001 = 34303;
double ML_LWR_THRESHOLD_SINGLE_PTO_2001 = 15970;
double ML_LWR_THRESHOLD_FAMILY_PTO_2001 = 31729;
double ML_TAPER_2001 = 0.20;
double ML_RATE_2001 = 0.015;
double LITO_MAX_OFFSET_2001 = 150;
double LITO_1ST_TAPER_2001 = -0.04;
double LITO_1ST_THRESH_2001 = 20700;
double SAPTO_MAX_SINGLE_2001 = 1608;
double SAPTO_MAX_MARRIED_2001 = 1155;
double SAPTO_LWR_SINGLE_2001 = 15459;
double SAPTO_LWR_MARRIED_2001 = 25590;
double SAPTO_TAPER_2001 = -0.125;
System System2001 = {
    .yr = 2001,
  .nb = 5,
  .BRACKETS = {0, 6000, 20000, 50000, 60000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
  .lwr_single = 13808,
  .upr_single = 16245,
  .lwr_family = 23300,
  .upr_family = 27412,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 20000,
  .upr_single_sapto = 23530,
  .lwr_family_sapto = 31729,
  .upr_family_sapto = 37329,
  .lwr_thr_up_per_child = 2140,
  .taper = 0.1,
  .rate = 0.015
  },
  .has_sapto = true,
  .S = {
  .year = 2001,
  .pension_age = 65,
  .mxo_single = 1608,
  .mxo_couple = 1155,
  .lwr_single = 15459,
  .lwr_couple = 25590,
  .upr_single = 28323,
  .upr_couple = 34830,
  .taper = 0.125,
  .first_tax_rate = 0.17,
  .second_tax_rate = 0.3,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 20000,
  .lito_max_offset = 150,
  .lito_1st_thresh = 20700,
  .lito_1st_taper = 0.04
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
