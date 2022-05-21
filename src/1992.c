double ML_LWR_THRESHOLD_SINGLE_1992 = 11746;
double ML_UPR_THRESHOLD_SINGLE_1992 = 12529;
double ML_LWR_THRESHOLD_FAMILY_1992 = 19675;
double ML_UPR_THRESHOLD_FAMILY_1992 = 20986;
double ML_LWR_THR_UP_PER_CHILD_1992 =  2100;
double ML_TAPER_1992 = 0.25;
double ML_RATE_1992 = 0.0125;
System System1992 = {
    .yr = 1992,
  .nb = 5,
  .BRACKETS = { 0, 5400, 20700, 36000, 50000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.2, 0.38, 0.46, 0.47, 0.47, 0.47, 0.47 },
  .M = {
  .lwr_single = 11746,
  .upr_single = 13819,
  .lwr_family = 19675,
  .upr_family = 23148,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 1992,
  .upr_single_sapto = 2344,
  .lwr_family_sapto = 16843008,
  .upr_family_sapto = 19815304,
  .lwr_thr_up_per_child = 2100,
  .taper = 0.25,
  .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
  .year = 1992,
  .pension_age = 65,
  .mxo_single = 0,
  .mxo_couple = 0,
  .lwr_single = 0,
  .lwr_couple = 0,
  .upr_single = 0,
  .upr_couple = 0,
  .taper = 0.125,
  .first_tax_rate = 0.2,
  .second_tax_rate = 0.38,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 20700,
  .lito_max_offset = NA,
  .lito_1st_thresh = NA,
  .lito_1st_taper = NA
  },
  .has_lito = false,
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
