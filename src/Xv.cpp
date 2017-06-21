// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
using namespace Rcpp;

inline void check_Xv(IntegerVector& Dim, NumericVector& y) {
  if (Dim[1] != y.size()) throw std::invalid_argument("Inconsistent dimension.");
}

inline void check_vX(IntegerVector& Dim, NumericVector& x) {
  if (Dim[0] != x.size()) throw std::invalid_argument("Inconsistent dimension.");
}

//[[Rcpp::export]]
SEXP Xv_dgCMatrix_numeric(S4 x, NumericVector y) {
  IntegerVector _i(x.slot("i")), _p(x.slot("p")), _Dim(x.slot("Dim"));
  check_Xv(_Dim, y);
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
SEXP Xv_dgCMatrix_numeric_folded(S4 x, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  IntegerVector _i(x.slot("i")), _p(x.slot("p")), _Dim(x.slot("Dim"));
  check_Xv(_Dim, v);
  NumericVector _x(x.slot("x"));
  NumericVector result(_Dim[0]);
  
}

//[[Rcpp::export]]
SEXP Xv_dgTMatrix_numeric(S4 x, NumericVector y) {
  IntegerVector _i(x.slot("i")), _j(x.slot("j")), _Dim(x.slot("Dim"));
  check_Xv(_Dim, y);
  NumericVector _x(x.slot("x"));
  NumericVector result(_Dim[0]);
  for(auto index = 0;index < _i.size();index++) {
    const auto row = _i[index];
    const auto col = _j[index];
    const auto value = _x[index];
    result[row] += y[col] * value;
  }
  return result;
}

//[[Rcpp::export]]
SEXP Xv_dgRMatrix_numeric(S4 x, NumericVector y) {
  IntegerVector _j(x.slot("j")), _p(x.slot("p")), _Dim(x.slot("Dim"));
  check_Xv(_Dim, y);
  NumericVector _x(x.slot("x"));
  NumericVector result(_Dim[0]);
  for(auto row = 0;row < _Dim[0];row++) {
    for(auto index = _p[row];index != _p[row + 1];index++) {
      const auto col = _j[index];
      const auto value = _x[index];
      result[row] += y[col] * value;
    }
  }
  return result;
}

//[[Rcpp::export]]
SEXP vX_numeric_dgCMatrix(NumericVector x, S4 y) {
  IntegerVector _i(y.slot("i")), _p(y.slot("p")), _Dim(y.slot("Dim"));
  check_vX(_Dim, x);
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

//[[Rcpp::export]]
SEXP vX_numeric_dgTMatrix(NumericVector x, S4 y) {
  IntegerVector _i(y.slot("i")), _j(y.slot("j")), _Dim(y.slot("Dim"));
  check_vX(_Dim, x);
  NumericVector _x(y.slot("x"));
  NumericVector result(_Dim[1]);
  for(auto index = 0;index < _i.size();index++) {
    const auto row = _i[index];
    const auto col = _j[index];
    const auto value = _x[index];
    result[col] += x[row] * value;
  }
  return result;
}

//[[Rcpp::export]]
SEXP vX_numeric_dgRMatrix(NumericVector x, S4 y) {
  IntegerVector _j(y.slot("j")), _p(y.slot("p")), _Dim(y.slot("Dim"));
  check_vX(_Dim, x);
  NumericVector _x(y.slot("x"));
  NumericVector result(_Dim[1]);
  for(auto row = 0;row < _Dim[0];row++) {
    for(auto index = _p[row];index != _p[row + 1];index++) {
      const auto col = _j[index];
      const auto value = _x[index];
      result[col] += x[row] * value;
    }
  }
  return result;
}

