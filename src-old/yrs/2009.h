#ifndef grattan_2009_H
#define grattan_2009_H

constexpr double ORD_TAX_BRACK_2009[5] = {0, 6000, 34e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2009[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2009 = 17794;
constexpr double ML_UPR_THRESHOLD_SINGLE_2009 = 20935;
constexpr double ML_LWR_THRESHOLD_FAMILY_2009 = 30025;
constexpr double ML_UPR_THRESHOLD_FAMILY_2009 = 35325;
constexpr double ML_LWR_THR_UP_PER_CHILD_2009 =  2757;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2009 = 28867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2009 = 33962;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2009 = 42000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2009 = 49413;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2009 = 25299;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2009 = 42000;
constexpr double ML_TAPER_2009 = 0.1;
constexpr double ML_RATE_2009 = 0.015;
double do_1_medicare_levy_2009(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2009 = 1200;
constexpr double LITO_1ST_TAPER_2009 = -0.04;
constexpr double LITO_1ST_THRESH_2009 = 30000;
constexpr double SAPTO_MAX_SINGLE_2009 = 2230;
constexpr double SAPTO_LWR_SINGLE_2009 = 28867;
constexpr double SAPTO_UPR_SINGLE_2009 = 46707;
constexpr double SAPTO_MAX_MARRIED_2009 = 2230;
constexpr double SAPTO_LWR_MARRIED_2009 = 28867;
constexpr double SAPTO_UPR_MARRIED_2009 = 46707;
constexpr double SAPTO_TAPER_2009 = -0.125;
#endif
