#ifndef grattan_2010_H
#define grattan_2010_H

constexpr double ORD_TAX_BRACK_2010[5] = {0, 6000, 35e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2010[5] = {0, 0.15, 0.3, 0.38, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2010 = 18488;
constexpr double ML_UPR_THRESHOLD_SINGLE_2010 = 21752;
constexpr double ML_LWR_THRESHOLD_FAMILY_2010 = 31196;
constexpr double ML_UPR_THRESHOLD_FAMILY_2010 = 36702;
constexpr double ML_LWR_THR_UP_PER_CHILD_2010 =  2865;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2010 = 29867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2010 = 35139;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2010 = 43500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2010 = 51178;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2010 = 27697;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2010 = 27697;
constexpr double ML_TAPER_2010 = 0.1;
constexpr double ML_RATE_2010 = 0.015;
double do_1_medicare_levy_2010(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2010 = 1350;
constexpr double LITO_1ST_TAPER_2010 = -0.04;
constexpr double LITO_1ST_THRESH_2010 = 30000;
constexpr double SAPTO_MAX_SINGLE_2010 = 2230;
constexpr double SAPTO_LWR_SINGLE_2010 = 29867;
constexpr double SAPTO_UPR_SINGLE_2010 = 47707;
constexpr double SAPTO_MAX_MARRIED_2010 = 2230;
constexpr double SAPTO_LWR_MARRIED_2010 = 29867;
constexpr double SAPTO_UPR_MARRIED_2010 = 47707;
constexpr double SAPTO_TAPER_2010 = -0.125;
#endif
