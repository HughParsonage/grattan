#ifndef grattan_2004_H
#define grattan_2004_H

constexpr double ORD_TAX_BRACK_2004[5] = {0, 6000, 21600, 52e3, 62500};
constexpr double ORD_TAX_RATES_2004[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2004 = 15529;
constexpr double ML_UPR_THRESHOLD_SINGLE_2004 = 16789;
constexpr double ML_LWR_THRESHOLD_FAMILY_2004 = 26205;
constexpr double ML_UPR_THRESHOLD_FAMILY_2004 = 28331;
constexpr double ML_LWR_THR_UP_PER_CHILD_2004 =  2406;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2004 = 20500;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2004 = 22163;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2004 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2004 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2004 = 18141;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2004 = 31729;
constexpr double ML_TAPER_2004 = 0.20;
constexpr double ML_RATE_2004 = 0.015;
double do_1_medicare_levy_2004(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2004 = 235;
constexpr double LITO_1ST_TAPER_2004 = -0.04;
constexpr double LITO_1ST_THRESH_2004 = 21600;
constexpr double SAPTO_MAX_SINGLE_2004 = 2230;
constexpr double SAPTO_MAX_MARRIED_2004 = 1602;
constexpr double SAPTO_LWR_SINGLE_2004 = 20500;
constexpr double SAPTO_LWR_MARRIED_2004 = 33612;
constexpr double PTO_MAX_SINGLE_2004 = 1925;
constexpr double PTO_MAX_MARRIED_2004 = 2848;
constexpr double PTO_LWR_SINGLE_2004 = 17342;
constexpr double PTO_LWR_MARRIED_2004 = 28754;
constexpr double SAPTO_TAPER_2004 = -0.125;
#endif
