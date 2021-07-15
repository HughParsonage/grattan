#ifndef grattan_2005_H
#define grattan_2005_H

constexpr double ORD_TAX_BRACK_2005[5] = {0, 6000, 21600, 58e3, 70e3};
constexpr double ORD_TAX_RATES_2005[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2005 = 15902;
constexpr double ML_UPR_THRESHOLD_SINGLE_2005 = 17192;
constexpr double ML_LWR_THRESHOLD_FAMILY_2005 = 26834;
constexpr double ML_UPR_THRESHOLD_FAMILY_2005 = 29011;
constexpr double ML_LWR_THR_UP_PER_CHILD_2005 =  2464;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2005 = 20500;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2005 = 22163;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2005 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2005 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2005 = 19252;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2005 = 31729;
constexpr double ML_TAPER_2005 = 0.1;
constexpr double ML_RATE_2005 = 0.015;
double do_1_medicare_levy_2005(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2005 = 235;
constexpr double LITO_1ST_TAPER_2005 = -0.04;
constexpr double LITO_1ST_THRESH_2005 = 21600;
constexpr double SAPTO_MAX_SINGLE_2005 = 2117;
constexpr double SAPTO_MAX_MARRIED_2005 = 3170;
constexpr double SAPTO_LWR_SINGLE_2005 = 18453;
constexpr double SAPTO_LWR_MARRIED_2005 = 30648;
constexpr double SAPTO_TAPER_2005 = -0.125;
#endif
