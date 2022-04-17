#ifndef grattan_2018_H
#define grattan_2018_H

constexpr double ORD_TAX_BRACK_2018[5] = {0, 18200, 37e3, 87e3, 180e3};
constexpr double ORD_TAX_RATES_2018[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2018 = 21980;
constexpr double ML_UPR_THRESHOLD_SINGLE_2018 = 27476;
constexpr double ML_LWR_THRESHOLD_FAMILY_2018 = 37089;
constexpr double ML_UPR_THRESHOLD_FAMILY_2018 = 46363;
constexpr double ML_LWR_THR_UP_PER_CHILD_2018 =  3406;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2018 = 34758;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2018 = 43449;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2018 = 48385;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2018 = 60483;
constexpr double ML_TAPER_2018 = 0.1;
constexpr double ML_RATE_2018 = 0.02;
double do_1_medicare_levy_2018(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2018 = 445;
constexpr double LITO_1ST_TAPER_2018 = -0.015;
constexpr double LITO_1ST_THRESH_2018 = 37000;
constexpr double SAPTO_MAX_SINGLE_2018 = 2230;
constexpr double SAPTO_MAX_MARRIED_2018 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2018 = 2040;
constexpr double SAPTO_TAPER_2018 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2018 = 32279;
constexpr double SAPTO_LWR_MARRIED_2018 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2018 = 28974;
constexpr double SBTO_DISCOUNT_2018 = 0.08;
#endif
