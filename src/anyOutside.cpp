#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int anyOutside(IntegerVector x, int a, int b) {
  int N = x.size();
  for (int i = 0; i < N; ++i) {
    if (x[i] < a || x[i] > b) {
      return ++i;
    }
  }
  return 0;
}
