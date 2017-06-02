#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
SEXP Xv_dgCMatrix_numeric(S4 x, NumericVector y) {
  IntegerVector _i(x.slot("i")), _p(x.slot("p")), _Dim(x.slot("Dim"));
  if (_Dim[1] != y.size()) throw std::invalid_argument("Inconsistent dimension.");
  NumericVector _x(x.slot("x"));
  NumericVector result(_Dim[0]);
  for(auto col = 0;col < _Dim[1];col++) {
    for(auto index = _p[col];index != _p[col + 1];index++) {
      const auto row = _i[index];
      const auto value = _x[index];
      result[row] += y[col] * value;
    }
  }
  return result;
}

//[[Rcpp::export]]
SEXP vX_numeric_dgCMatrix(NumericVector x, S4 y) {
  IntegerVector _i(y.slot("i")), _p(y.slot("p")), _Dim(y.slot("Dim"));
  if (_Dim[0] != x.size()) throw std::invalid_argument("Inconsistent dimension.");
  NumericVector _x(y.slot("x"));
  NumericVector result(_Dim[1]);
  for(auto col = 0;col < _Dim[1];col++) {
    for(auto index = _p[col];index != _p[col + 1];index++) {
      const auto row = _i[index];
      const auto value = _x[index];
      result[col] += x[row] * value;
    }
  }
  return result;
}
