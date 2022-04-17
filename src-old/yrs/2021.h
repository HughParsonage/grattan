#ifndef grattan_2021_H
#define grattan_2021_H

constexpr double ORD_TAX_BRACK_2021[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2021[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2021 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2021 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2021 = 38474; // Constant from here on
constexpr double ML_UPR_THRESHOLD_FAMILY_2021 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2021 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2021 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2021 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2021 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2021 = 62740;
constexpr double ML_TAPER_2021 = 0.1;
constexpr double ML_RATE_2021 = 0.02;
double do_1_medicare_levy_2021(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2021 = 445;
constexpr double LITO_1ST_TAPER_2021 = -0.015;
constexpr double LITO_1ST_THRESH_2021 = 37000;
constexpr double SAPTO_MAX_SINGLE_2021 = 2230;
constexpr double SAPTO_MAX_MARRIED_2021 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2021 = 2040;
constexpr double SAPTO_TAPER_2021 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2021 = 32279;
constexpr double SAPTO_LWR_MARRIED_2021 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2021 = 28974;
constexpr double LMITO_1ST_OFFSET_2021 = 255;
constexpr double LMITO_THRESHOLDS_2021[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES_2021[4] = {0.075, 0, -0.03, 0};
constexpr double SBTO_DISCOUNT_2021 = 0.08;
#endif
