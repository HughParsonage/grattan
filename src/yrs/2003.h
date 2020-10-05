#ifndef grattan_2003_H
#define grattan_2003_H

constexpr double ORD_TAX_BRACK_2003[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2003[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2003 = 15062;
constexpr double ML_UPR_THRESHOLD_SINGLE_2003 = 16284;
constexpr double ML_LWR_THRESHOLD_FAMILY_2003 = 25417;
constexpr double ML_UPR_THRESHOLD_FAMILY_2003 = 27479;
constexpr double ML_LWR_THR_UP_PER_CHILD_2003 =  2334;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2003 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2003 = 21623;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2003 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2003 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2003 = 17164;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2003 = 31729;
constexpr double ML_TAPER_2003 = 0.20;
constexpr double ML_RATE_2003 = 0.015;
double do_1_medicare_levy_2003(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2003 = 150;
constexpr double LITO_1ST_TAPER_2003 = -0.04;
constexpr double LITO_1ST_THRESH_2003 = 18575;
constexpr double SAPTO_MAX_SINGLE_2003 = 1811;
constexpr double SAPTO_MAX_MARRIED_2003 = 2648;
constexpr double SAPTO_LWR_SINGLE_2003 = 16653;
constexpr double SAPTO_LWR_MARRIED_2003 = 27578;
constexpr double SAPTO_TAPER_2003 = -0.125;
#endif
