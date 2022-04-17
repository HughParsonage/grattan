#ifndef grattan_2001_H
#define grattan_2001_H

constexpr double ORD_TAX_BRACK_2001[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2001[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2001 = 13808;
constexpr double ML_UPR_THRESHOLD_SINGLE_2001 = 14927;
constexpr double ML_LWR_THRESHOLD_FAMILY_2001 = 23300;
constexpr double ML_UPR_THRESHOLD_FAMILY_2001 = 25190;
constexpr double ML_LWR_THR_UP_PER_CHILD_2001 =  2140;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2001 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2001 = 21623;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2001 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2001 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2001 = 15970;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2001 = 31729;
constexpr double ML_TAPER_2001 = 0.20;
constexpr double ML_RATE_2001 = 0.015;
double do_1_medicare_levy_2001(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2001 = 150;
constexpr double LITO_1ST_TAPER_2001 = -0.04;
constexpr double LITO_1ST_THRESH_2001 = 20700;
constexpr double SAPTO_MAX_SINGLE_2001 = 1608;
constexpr double SAPTO_MAX_MARRIED_2001 = 1155;
constexpr double SAPTO_LWR_SINGLE_2001 = 15459;
constexpr double SAPTO_LWR_MARRIED_2001 = 25590;
constexpr double SAPTO_TAPER_2001 = -0.125;
#endif
