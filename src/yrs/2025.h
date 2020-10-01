#ifndef grattan_2025_H
#define grattan_2025_H

constexpr double ORD_TAX_BRACK_2025[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2025[4] = {0, 0.190, 0.300, 0.450};
constexpr double ML_LWR_THRESHOLD_SINGLE_2025 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2025 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2025 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2025 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2025 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2025 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2025 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2025 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2025 = 62740;
constexpr double ML_TAPER_2025 = 0.1;
constexpr double ML_RATE_2025 = 0.02;
double do_1_medicare_levy_2025(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2025 = 2230;
constexpr double SAPTO_MAX_MARRIED_2025 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2025 = 2040;
constexpr double SAPTO_TAPER_2025 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2025 = 33622;
constexpr double SAPTO_LWR_MARRIED_2025 = 30316;
constexpr double SAPTO_LWR_ILL_SEP_2025 = 32622;
constexpr double LITO_MAX_OFFSET_2025 = 700;
constexpr double LITO_1ST_THRESH_2025 = 37500;
constexpr double LITO_2ND_THRESH_2025 = 45000;
constexpr double LITO_1ST_TAPER_2025 = -0.050;
constexpr double LITO_2ND_TAPER_2025 = -0.015;
constexpr double SBTO_DISCOUNT_2025 = 0.1;
#endif
