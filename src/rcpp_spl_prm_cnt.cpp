#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix spl_prm_cnt(IntegerMatrix x, int n) {
  int o = x.rows();
  int m = x.cols();
  NumericMatrix z(o*n, m);
  for (int k = 0; k < o; ++k) {
    for (int j = 0; j < m; ++j) {
      int quo = x[j] / 12;
      int rem = x[j] % 12;
      for (int i = k*n; i < (k+1)*n; ++i) {
        if (i-k*n < quo) {
          z(i, j) = 12;
        }
        else if (i-k*n == quo) {
          z(i, j) = rem;
        }
      }
    }
  }
  return z;
}

