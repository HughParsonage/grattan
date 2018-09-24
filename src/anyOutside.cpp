#include <Rcpp.h>
using namespace Rcpp;



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
