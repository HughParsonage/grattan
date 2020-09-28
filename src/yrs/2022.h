#ifndef grattan_2022_H
#define grattan_2022_H

constexpr double ORD_TAX_BRACK_2022[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2022[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2022 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2022 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2022 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2022 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2022 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2022 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2022 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2022 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2022 = 62740;
double do_1_medicare_levy_2022(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2022 = 2230;
constexpr double SAPTO_MAX_MARRIED_2022 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2022 = 2040;
constexpr double SAPTO_TAPER_2022 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2022 = 32279;
constexpr double SAPTO_LWR_MARRIED_2022 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2022 = 28974;
constexpr double LMITO_1ST_OFFSET_2022 = 255;
constexpr double LMITO_THRESHOLDS_2022[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES_2022[4] = {0.075, 0, -0.03, 0};
constexpr double LITO_MAX_OFFSET_2022 = 445;
constexpr double LITO_1ST_THRESH_2022 = 37000;
constexpr double LITO_1ST_TAPER_2022 = -0.015;
#endif
