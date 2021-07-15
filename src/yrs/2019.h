#ifndef grattan_2019_H
#define grattan_2019_H

constexpr double ORD_TAX_BRACK_2019[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2019[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2019 = 22398;
constexpr double ML_UPR_THRESHOLD_SINGLE_2019 = 27999;
constexpr double ML_LWR_THRESHOLD_FAMILY_2019 = 37794;
constexpr double ML_UPR_THRESHOLD_FAMILY_2019 = 47244;
constexpr double ML_LWR_THR_UP_PER_CHILD_2019 =  3471;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2019 = 35418;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2019 = 44274;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2019 = 49304;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2019 = 61631;
constexpr double ML_TAPER_2019 = 0.1;
constexpr double ML_RATE_2019 = 0.02;
double do_1_medicare_levy_2019(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2019 = 445;
constexpr double LITO_1ST_TAPER_2019 = -0.015;
constexpr double LITO_1ST_THRESH_2019 = 37000;
constexpr double SAPTO_MAX_SINGLE_2019 = 2230;
constexpr double SAPTO_MAX_MARRIED_2019 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2019 = 2040;
constexpr double SAPTO_TAPER_2019 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2019 = 32279;
constexpr double SAPTO_LWR_MARRIED_2019 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2019 = 28974;
constexpr double LMITO_1ST_OFFSET_2019 = 255;
constexpr double LMITO_THRESHOLDS_2019[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES_2019[4] = {0.075, 0, -0.03, 0};
constexpr double SBTO_DISCOUNT_2019 = 0.08;
#endif
