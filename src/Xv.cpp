#include <Xv.h>
#include <Rcpp.h>
using namespace Rcpp;

template<typename F>
SEXP Xv_number(S4 x, NumericVector y, F& f) {
  IntegerVector _Dim(x.slot("Dim"));
  Xv::check_Xv(_Dim, y);
  NumericVector result(_Dim[0]);
  f(x, y, result, nullptr, 0, true);
  return result;
} 

//[[Rcpp::export]]
SEXP Xv_dgCMatrix_numeric(S4 x, NumericVector y) {
  return Xv_number(x, y, Xv::Xv_dgCMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export]]
SEXP Xv_dgTMatrix_numeric(S4 x, NumericVector y) {
  return Xv_number(x, y, Xv::Xv_dgTMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export]]
SEXP Xv_dgRMatrix_numeric(S4 x, NumericVector y) {
  return Xv_number(x, y, Xv::Xv_dgRMatrix_numeric_folded<NumericVector, NumericVector>);
}

template<typename F>
SEXP Xv_number_folded(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude, F& f) {
  IntegerVector _Dim(X.slot("Dim"));
  Xv::check_Xv(_Dim, v);
  std::vector<double> result(_Dim[0], 0.0);
  std::size_t size = f(X, v, result, foldid, target, is_exclude);
  result.resize(size);
  return wrap(result);
}

//[[Rcpp::export("Xv_dgCMatrix_numeric_folded")]]
SEXP _Xv_dgCMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return Xv_number_folded(X, v, foldid, target, is_exclude, Xv::Xv_dgCMatrix_numeric_folded<NumericVector, std::vector<double> >);
}

//[[Rcpp::export("Xv_dgTMatrix_numeric_folded")]]
SEXP _Xv_dgTMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return Xv_number_folded(X, v, foldid, target, is_exclude, Xv::Xv_dgTMatrix_numeric_folded<NumericVector, std::vector<double> >);
}

//[[Rcpp::export("Xv_dgRMatrix_numeric_folded")]]
SEXP _Xv_dgRMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return Xv_number_folded(X, v, foldid, target, is_exclude, Xv::Xv_dgRMatrix_numeric_folded<NumericVector, std::vector<double> >);
}


//[[Rcpp::export]]
SEXP vX_numeric_dgCMatrix(NumericVector x, S4 y) {
  IntegerVector _i(y.slot("i")), _p(y.slot("p")), _Dim(y.slot("Dim"));
  Xv::check_vX(_Dim, x);
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

//[[Rcpp::export("vX_dgCMatrix_numeric_folded")]]
SEXP _vX_dgCMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  Rcout << "Calling vX_dgCMatrix_numeric_folded" << std::endl;
  return R_NilValue;
}


//[[Rcpp::export]]
SEXP vX_numeric_dgTMatrix(NumericVector x, S4 y) {
  IntegerVector _i(y.slot("i")), _j(y.slot("j")), _Dim(y.slot("Dim"));
  Xv::check_vX(_Dim, x);
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

//[[Rcpp::export("vX_dgTMatrix_numeric_folded")]]
SEXP _vX_dgTMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  Rcout << "Calling vX_dgTMatrix_numeric_folded" << std::endl;
  return R_NilValue;
}


//[[Rcpp::export]]
SEXP vX_numeric_dgRMatrix(NumericVector x, S4 y) {
  IntegerVector _j(y.slot("j")), _p(y.slot("p")), _Dim(y.slot("Dim"));
  Xv::check_vX(_Dim, x);
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

//[[Rcpp::export("vX_dgRMatrix_numeric_folded")]]
SEXP _vX_dgRMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  Rcout << "Calling vX_dgRMatrix_numeric_folded" << std::endl;
  return R_NilValue;
}

