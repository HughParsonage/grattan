#ifndef grattan_2016_H
#define grattan_2016_H

constexpr double ORD_TAX_BRACK_2016[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2016[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2016 = 21335;
constexpr double ML_UPR_THRESHOLD_SINGLE_2016 = 26670;
constexpr double ML_LWR_THRESHOLD_FAMILY_2016 = 35261;
constexpr double ML_UPR_THRESHOLD_FAMILY_2016 = 44078;
constexpr double ML_LWR_THR_UP_PER_CHILD_2016 =  3306;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2016 = 33738;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2016 = 42174;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2016 = 46966;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2016 = 58709;
constexpr double ML_TAPER_2016 = 0.1;
constexpr double ML_RATE_2016 = 0.02;
double do_1_medicare_levy_2016(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2016 = 445;
constexpr double LITO_1ST_TAPER_2016 = -0.015;
constexpr double LITO_1ST_THRESH_2016 = 37000;
constexpr double SAPTO_MAX_SINGLE_2016 = 2230;
constexpr double SAPTO_MAX_MARRIED_2016 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2016 = 2040;
constexpr double SAPTO_TAPER_2016 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2016 = 32279;
constexpr double SAPTO_LWR_MARRIED_2016 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2016 = 28974;
constexpr double SBTO_DISCOUNT_2016 = 0.05;
#endif
