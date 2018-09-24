//' @title Parallel maximum
//' @description A faster \code{pmax()}.
//'
//' @name pmaxC
//' @param x A numeric vector.
//' @param a A single numeric value.
//' @return The parallel maximum of the input values. \code{pmax0(x)} is
//'  shorthand for \code{pmaxC(x, 0)}, i.e. convert negative values in \code{x} to 0.
//' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is
//'  a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short.
//'  Use this function when comparing a numeric vector with a single value.
//' @export pmaxC

#include <Rcpp.h>
using namespace Rcpp;
// #include <RcppParallel.h>
// using namespace Rcpp;
// 
// struct Pmax : public RcppParallel::Worker {
//   struct Apply {
//     double mx;
//     Apply(double mx_)
//       : mx(mx_)
//     {}
//     
//     double operator()(const double x) const 
//     {
//       return x > mx ? x : mx;
//     }
//   };
//   
//   const RcppParallel::RVector<double> input;
//   RcppParallel::RVector<double> output;
//   
//   Apply f; 
//   
//   Pmax(const Rcpp::NumericVector input_,
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
// // [[Rcpp::export]]
// Rcpp::NumericVector pmaxC(Rcpp::NumericVector x, double a)
// {
//   Rcpp::NumericVector res = Rcpp::no_init_vector(x.size());
//   Pmax p(x, res, a);
//   RcppParallel::parallelFor(0, x.size(), p);
//   
//   return res;
// } 

// [[Rcpp::export]]
NumericVector pmaxC(NumericVector x, double a) {
  int n = x.length();
  NumericVector out(n);

  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi < a) {
      out[i] = a;
    } else {
      out[i] = xi;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector pmax0(NumericVector x) {
  int n = x.length();
  NumericVector out(n);
 
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi > 0) {
      out[i] = xi;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector pmaxIPnum0(NumericVector x) {
  int n = x.length();
  
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0) {
      x[i] = 0;
    }
  }
  return x;
}

// [[Rcpp::export]]
IntegerVector pmaxIPint0(IntegerVector x) {
  int n = x.length();
  
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0) {
      x[i] = 0;
    }
  }
  return x;
}


