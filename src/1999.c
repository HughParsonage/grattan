double ML_LWR_THRESHOLD_SINGLE_1999 = 13390;
double ML_UPR_THRESHOLD_SINGLE_1999 = 14474;
double ML_LWR_THRESHOLD_FAMILY_1999 = 22595;
double ML_UPR_THRESHOLD_FAMILY_1999 = 24425;
double ML_LWR_THR_UP_PER_CHILD_1999 =  2100;
double ML_TAPER_1999 = 0.20;
double ML_RATE_1999 = 0.015;
double LITO_MAX_OFFSET_1999 = 150;
double LITO_1ST_TAPER_1999 = -0.04;
double LITO_1ST_THRESH_1999 = 20700;
System System1999 = {
    .yr = 1999,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 38000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.34, 0.43, 0.47, 0.47, 0.47, 0.47 },
  .M = {
  .lwr_single = 13390,
  .upr_single = 15753,
  .lwr_family = 22595,
  .upr_family = 26583,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 1999,
  .upr_single_sapto = 2352,
  .lwr_family_sapto = 16843008,
  .upr_family_sapto = 19815304,
  .lwr_thr_up_per_child = 2100,
  .taper = 0.2,
  .rate = 0.015
  },
  .has_sapto = false,
  .S = {
  .year = 1999,
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
  .lito_max_offset = NA,
  .lito_1st_thresh = NA,
  .lito_1st_taper = NA
  },
  .has_lito = true,
  .has_lmito = false,
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
