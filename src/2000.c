#ifndef grattan_2000_H
#define grattan_2000_H
#include "grattan.h"
double ML_LWR_THRESHOLD_SINGLE_2000 = 13551;
double ML_UPR_THRESHOLD_SINGLE_2000 = 14648;
double ML_LWR_THRESHOLD_FAMILY_2000 = 22866;
double ML_UPR_THRESHOLD_FAMILY_2000 = 24718;
double ML_LWR_THR_UP_PER_CHILD_2000 =  2100;
double ML_TAPER_2000 = 0.20;
double ML_RATE_2000 = 0.015;
double SAPTO_MAX_SINGLE_2000 = 1358;
double SAPTO_MAX_MARRIED_2000 = 1960;
double SAPTO_LWR_SINGLE_2000 = 12190;
double SAPTO_LWR_MARRIED_2000 = 20600;
double SAPTO_TAPER_2000 = -0.125;
double LITO_MAX_OFFSET_2000 = 150;
double LITO_1ST_TAPER_2000 = -0.04;
double LITO_1ST_THRESH_2000 = 20700;
System System2000 = {
    .yr = 2000,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
  .lwr_single = 13551,
  .upr_single = 15943,
  .lwr_family = 22866,
  .upr_family = 26902,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 1264515296,
  .upr_single_sapto = 1487662231,
  .lwr_family_sapto = 16843008,
  .upr_family_sapto = 19815304,
  .lwr_thr_up_per_child = 2100,
  .taper = 0.2,
  .rate = 0.015
  },
  .has_sapto = false,
  .S = {
  .year = 2000,
  .pension_age = 65,
  .mxo_single = 1608,
  .mxo_couple = 1155,
  .lwr_single = 15459,
  .lwr_couple = 25590,
  .upr_single = 28323,
  .upr_couple = 34830,
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
  .Thresholds = 20700,
  .Tapers = -0.04,
  .nb = 1,
  .refundable = false
}
},
  .has_temp_budget_repair_levy = false
};
#endif
