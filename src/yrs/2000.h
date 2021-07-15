#ifndef grattan_2000_H
#define grattan_2000_H

constexpr double ORD_TAX_BRACK_2000[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_2000[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2000 = 13551;
constexpr double ML_UPR_THRESHOLD_SINGLE_2000 = 14648;
constexpr double ML_LWR_THRESHOLD_FAMILY_2000 = 22866;
constexpr double ML_UPR_THRESHOLD_FAMILY_2000 = 24718;
constexpr double ML_LWR_THR_UP_PER_CHILD_2000 =  2100;
constexpr double ML_TAPER_2000 = 0.20;
constexpr double ML_RATE_2000 = 0.015;
double do_1_medicare_levy_2000(double xd, double yd, bool is_family, int n_dependants );
constexpr double SAPTO_MAX_SINGLE_2000 = 1358;
constexpr double SAPTO_MAX_MARRIED_2000 = 1960;
constexpr double SAPTO_LWR_SINGLE_2000 = 12190;
constexpr double SAPTO_LWR_MARRIED_2000 = 20600;
constexpr double SAPTO_TAPER_2000 = -0.125;
constexpr double LITO_MAX_OFFSET_2000 = 150;
constexpr double LITO_1ST_TAPER_2000 = -0.04;
constexpr double LITO_1ST_THRESH_2000 = 20700;
#endif
