#ifndef grattan_H
#define grattan_H

double sapto_rcpp_singleton(double rebate_income, double max_offset, double lower_threshold, double taper_rate, bool sapto_eligible, double Spouse_income, std::string family_status);
Rcpp::NumericVector pminC(Rcpp::NumericVector x, double a);
double sapto_rcpp_singleton(double rebate_income,double max_offset,double lower_threshold,double taper_rate,bool sapto_eligible = true,double Spouse_income = 0,std::string family_status = "single");

#endif