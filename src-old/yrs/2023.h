#ifndef grattan_2023_H
#define grattan_2023_H

constexpr double ORD_TAX_BRACK_2023[5] = {0, 18200, 45e3, 120e3, 180e3};
constexpr double ORD_TAX_RATES_2023[5] = {0, 0.190, 0.325, 0.370, 0.450};
constexpr double ML_LWR_THRESHOLD_SINGLE_2023 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2023 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2023 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2023 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2023 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2023 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2023 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2023 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2023 = 62740;
constexpr double ML_TAPER_2023 = 0.1;
constexpr double ML_RATE_2023 = 0.02;
double do_1_medicare_levy_2023(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2023 = 2230;
constexpr double SAPTO_MAX_MARRIED_2023 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2023 = 2040;
constexpr double SAPTO_TAPER_2023 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2023 = 33622;
constexpr double SAPTO_LWR_MARRIED_2023 = 30316;
constexpr double SAPTO_LWR_ILL_SEP_2023 = 32622;
constexpr double LITO_MAX_OFFSET_2023 = 700;
constexpr double LITO_1ST_THRESH_2023 = 37500;
constexpr double LITO_2ND_THRESH_2023 = 45000;
constexpr double LITO_1ST_TAPER_2023 = -0.050;
constexpr double LITO_2ND_TAPER_2023 = -0.015;

constexpr double SBTO_DISCOUNT_2023 = 0.08;
#endif
