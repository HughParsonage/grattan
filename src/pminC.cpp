//' @title Parallel maximum
//' @description A faster \code{pmin()}.
//'
//' @name pminC
//' @param x A numeric vector.
//' @param a A single numeric value.
//' @return The parallel minimum of the input values. The \code{0} versions are shortcuts for \code{a = 0}.
//' @note This function will always be faster than \code{pmin(x, a)} when \code{a} is a single value, but can be slower than \code{pmin.int(x, a)} when \code{x} is short. Use this function when comparing a numeric vector with a single value.
//' @export pminC


#include <Rcpp.h>
// #include <RcppParallel.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pminC(NumericVector x, double a) {
  int n = x.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi > a) {
      out[i] = a;
    } else {
      out[i] = xi;
    }
  }
  
  return out;
}

// [[Rcpp::export]]
NumericVector pmin0(NumericVector x) {
  int n = x.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi < 0) {
      out[i] = xi;
    }
  }
  
  return out;
}

// struct Pmin : public RcppParallel::Worker {
//   struct Apply {
//     double mx;
//     Apply(double mx_)
//       : mx(mx_)
//     {}
//     
//     double operator()(const double x) const 
//     {
//       return x < mx ? x : mx;
//     }
//   };
//   
//   const RcppParallel::RVector<double> input;
//   RcppParallel::RVector<double> output;
//   
//   Apply f; 
//   
//   Pmin(const Rcpp::NumericVector input_,
//        Rcpp::NumericVector output_,
//        double mx_) 
//     : input(input_), output(output_), f(mx_)
//   {}
//   
//   void operator()(std::size_t begin, std::size_t end)
//   {
//     std::transform(
//       input.begin() + begin,
//       input.begin() + end,
//       output.begin() + begin,
//       f
//     );
//   }
// };
// 
// 
// Rcpp::NumericVector pminC(Rcpp::NumericVector x, double a)
// {
//   Rcpp::NumericVector res = Rcpp::no_init_vector(x.size());
//   Pmin p(x, res, a);
//   RcppParallel::parallelFor(0, x.size(), p);
//   
//   return res;
// }

