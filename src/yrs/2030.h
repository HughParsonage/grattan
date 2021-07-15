#ifndef grattan_2030_H
#define grattan_2030_H

constexpr double ORD_TAX_BRACK_2030[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2030[4] = {0, 0.190, 0.300, 0.450};
constexpr double ML_LWR_THRESHOLD_SINGLE_2030 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2030 = 28503;
constexpr double ML_LWR_THRESHOLD_FAMILY_2030 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2030 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2030 =  3533;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2030 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2030 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2030 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2030 = 62740;
constexpr double ML_TAPER_2030 = 0.1;
constexpr double ML_RATE_2030 = 0.02;
double do_1_medicare_levy_2030(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2030 = 2230;
constexpr double SAPTO_MAX_MARRIED_2030 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2030 = 2040;
constexpr double SAPTO_TAPER_2030 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2030 = 33622;
constexpr double SAPTO_LWR_MARRIED_2030 = 30316;
constexpr double SAPTO_LWR_ILL_SEP_2030 = 32622;
constexpr double LITO_MAX_OFFSET_2030 = 700;
constexpr double LITO_1ST_THRESH_2030 = 37500;
constexpr double LITO_2ND_THRESH_2030 = 45000;
constexpr double LITO_1ST_TAPER_2030 = -0.050;
constexpr double LITO_2ND_TAPER_2030 = -0.015;
constexpr double SBTO_DISCOUNT_2030 = 0.16;
#endif
