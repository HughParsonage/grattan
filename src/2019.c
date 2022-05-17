#ifndef grattan_2019_H
#define grattan_2019_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2019 = 22398;
double ML_UPR_THRESHOLD_SINGLE_2019 = 27999;
double ML_LWR_THRESHOLD_FAMILY_2019 = 37794;
double ML_UPR_THRESHOLD_FAMILY_2019 = 47244;
double ML_LWR_THR_UP_PER_CHILD_2019 =  3471;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2019 = 35418;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2019 = 44274;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2019 = 49304;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2019 = 61631;
double ML_TAPER_2019 = 0.1;
double ML_RATE_2019 = 0.02;
double LITO_MAX_OFFSET_2019 = 445;
double LITO_1ST_TAPER_2019 = -0.015;
double LITO_1ST_THRESH_2019 = 37000;
double SAPTO_MAX_SINGLE_2019 = 2230;
double SAPTO_MAX_MARRIED_2019 = 1602;
double SAPTO_MAX_ILL_SEP_2019 = 2040;
double SAPTO_TAPER_2019 = -0.125;
double SAPTO_LWR_SINGLE_2019 = 32279;
double SAPTO_LWR_MARRIED_2019 = 28974;
double SAPTO_LWR_ILL_SEP_2019 = 28974;
double LMITO_1ST_OFFSET_2019 = 255;
double SBTO_DISCOUNT_2019 = 0.08;

System System2019 = {
  .yr = 2019,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 90000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = 
{
    .lwr_single = 22398,
    .upr_single = 27999,
    .lwr_family = 37794,
    .upr_family = 47244,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 35418,
    .upr_single_sapto = 44274,
    .lwr_family_sapto = 49304,
    .upr_family_sapto = 61631,
    .lwr_thr_up_per_child = 3471,
    .taper = 0.1,
    .rate = 0.02
},
.has_sapto = true,
.S = {
.year = 2019,
.pension_age = 65,
.mxo_single = 2230,
.mxo_couple = 1602,
.lwr_single = 32279,
.lwr_couple = 28974,
.upr_single = 50119,
.upr_couple = 41790,
.taper = 0.125,
.first_tax_rate = 0.19,
.second_tax_rate = 0.325,
.tax_free_thresh = 6000,
.tax_2nd_thresh = 37000,
.lito_max_offset = 445,
.lito_1st_thresh = 37000,
.lito_1st_taper = 0.015
},
.has_lito = true,
.has_lmito = true,
.has_offset1 = false,
.O1 = {
.offset_1st = 0,
.thresh_1st = 0,
.taper_1st = 0,
.refundable = false
},
.has_offset2 = false,
.O2 = {
.offset_1st = 0,
.thresh_1st = 0,
.taper_1st = 0,
.thresh_2nd = 0,
.taper_2nd = 0,
.refundable = 0
},
.has_offsetn = false,
.Offsets = {
.offset_1st = 0,
.Thresholds = {0, 0, 0, 0, 0, 0, 0, 0},
.Tapers = {0, 0, 0, 0, 0, 0, 0, 0},
.nb = 0,
.refundable = false,
},
.has_temp_budget_repair_levy = false
};

#endif