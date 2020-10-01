#ifndef grattan_2024_H
#define grattan_2024_H

constexpr double ORD_TAX_BRACK_2024[5] = {0, 18200, 45e3, 120e3, 180e3};
constexpr double ORD_TAX_RATES_2024[5] = {0, 0.190, 0.325, 0.370, 0.450};
constexpr double ML_LWR_THRESHOLD_SINGLE_2024 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2024 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2024 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2024 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2024 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2024 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2024 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2024 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2024 = 62740;
constexpr double ML_TAPER_2024 = 0.1;
constexpr double ML_RATE_2024 = 0.02;
double do_1_medicare_levy_2024(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2024 = 2230;
constexpr double SAPTO_MAX_MARRIED_2024 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2024 = 2040;
constexpr double SAPTO_TAPER_2024 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2024 = 33622;
constexpr double SAPTO_LWR_MARRIED_2024 = 30316;
constexpr double SAPTO_LWR_ILL_SEP_2024 = 32622;
constexpr double LITO_MAX_OFFSET_2024 = 700;
constexpr double LITO_1ST_THRESH_2024 = 37500;
constexpr double LITO_2ND_THRESH_2024 = 45000;
constexpr double LITO_1ST_TAPER_2024 = -0.050;
constexpr double LITO_2ND_TAPER_2024 = -0.015;
constexpr double SBTO_DISCOUNT_2024 = 0.08;
#endif
