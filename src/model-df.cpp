#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double model_df(NumericMatrix x, NumericMatrix z) {
  double df = 0;
  for(int i = 0; i < x.nrow(); i++) {
    for(int j = 0; j < x.ncol(); j++) {
      df += x(i, j) * z(j, i);
    }
  }
  return df;
}
