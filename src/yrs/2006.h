#ifndef grattan_2006_H
#define grattan_2006_H

constexpr double ORD_TAX_BRACK_2006[5] = {0, 6000, 21600, 63e3, 95e3};
constexpr double ORD_TAX_RATES_2006[5] = {0, 0.15, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2006 = 16284;
constexpr double ML_UPR_THRESHOLD_SINGLE_2006 = 17605;
constexpr double ML_LWR_THRESHOLD_FAMILY_2006 = 27478;
constexpr double ML_UPR_THRESHOLD_FAMILY_2006 = 29707;
constexpr double ML_LWR_THR_UP_PER_CHILD_2006 =  2523;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2006 = 21968;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2006 = 23750;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2006 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2006 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2006 = 19583;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2006 = 31729;
constexpr double ML_TAPER_2006 = 0.1;
constexpr double ML_RATE_2006 = 0.015;
double do_1_medicare_levy_2006(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2006 = 235;
constexpr double LITO_1ST_TAPER_2006 = -0.04;
constexpr double LITO_1ST_THRESH_2006 = 21600;
constexpr double SAPTO_MAX_MARRIED_2006 = 3204;
constexpr double SAPTO_LWR_MARRIED_2006 = 36494;
constexpr double SAPTO_UPR_MARRIED_2006 = 31063;
constexpr double SAPTO_MAX_SINGLE_2006 = 2230;
constexpr double SAPTO_LWR_SINGLE_2006 = 21968;
constexpr double SAPTO_UPR_SINGLE_2006 = 39808;
constexpr double SAPTO_TAPER_2006 = -0.125;
#endif
