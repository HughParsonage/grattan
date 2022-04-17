#ifndef grattan_2020_H
#define grattan_2020_H

constexpr double ORD_TAX_BRACK_2020[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2020[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2020 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2020 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2020 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2020 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2020 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2020 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2020 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2020 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2020 = 62740;
constexpr double ML_TAPER_2020 = 0.1;
constexpr double ML_RATE_2020 = 0.02;
double do_1_medicare_levy_2020(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2020 = 445;
constexpr double LITO_1ST_TAPER_2020 = -0.015;
constexpr double LITO_1ST_THRESH_2020 = 37000;
constexpr double SAPTO_MAX_SINGLE_2020 = 2230;
constexpr double SAPTO_MAX_MARRIED_2020 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2020 = 2040;
constexpr double SAPTO_TAPER_2020 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2020 = 32279;
constexpr double SAPTO_LWR_MARRIED_2020 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2020 = 28974;
constexpr double LMITO_1ST_OFFSET_2020 = 255;
constexpr double LMITO_THRESHOLDS_2020[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES_2020[4] = {0.075, 0, -0.03, 0};
constexpr double SBTO_DISCOUNT_2020 = 0.08;
#endif
