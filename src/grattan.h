#ifndef grattan_H
#define grattan_H

double sapto_rcpp_singleton(double rebate_income,
                            double max_offset,
                            double lower_threshold,
                            double threshold_rate,
                            bool sapto_eligible = true,
                            double Spouse_income = 0,
                            char family_status = "single");

#endif