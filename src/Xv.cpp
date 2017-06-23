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

template<typename F>
SEXP vX_number(NumericVector x, S4 y, F& f) {
  IntegerVector _Dim(y.slot("Dim"));
  Xv::check_vX(_Dim, x);
  NumericVector result(_Dim[1]);
  f(y, x, result, nullptr, 0, true, nullptr);
  return result;
} 

//[[Rcpp::export]]
SEXP vX_numeric_dgCMatrix(NumericVector x, S4 y) {
  return vX_number(x, y, Xv::vX_dgCMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export]]
SEXP vX_numeric_dgTMatrix(NumericVector x, S4 y) {
  return vX_number(x, y, Xv::vX_dgTMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export]]
SEXP vX_numeric_dgRMatrix(NumericVector x, S4 y) {
  return vX_number(x, y, Xv::vX_dgRMatrix_numeric_folded<NumericVector, NumericVector>);
}

template<typename F>
SEXP vX_number_folded(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude, F& f) {
  IntegerVector _Dim(X.slot("Dim"));
  NumericVector result(_Dim[1]);
  f(X, v, result, foldid, target, is_exclude, nullptr);
  return result;
}

//[[Rcpp::export("vX_dgCMatrix_numeric_folded")]]
SEXP _vX_dgCMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return vX_number_folded(X, v, foldid, target, is_exclude, Xv::vX_dgCMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export("vX_dgTMatrix_numeric_folded")]]
SEXP _vX_dgTMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return vX_number_folded(X, v, foldid, target, is_exclude, Xv::vX_dgTMatrix_numeric_folded<NumericVector, NumericVector>);
}

//[[Rcpp::export("vX_dgRMatrix_numeric_folded")]]
SEXP _vX_dgRMatrix_numeric(S4 X, NumericVector v, IntegerVector foldid, int target, bool is_exclude) {
  return vX_number_folded(X, v, foldid, target, is_exclude, Xv::vX_dgRMatrix_numeric_folded<NumericVector, NumericVector>);
}
