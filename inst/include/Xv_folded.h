#ifndef __XV_FOLDED_H__
#define __XV_FOLDED_H__

#include <cstddef>
#include <stdexcept>
#include <vector>
#ifdef XV_DEBUG
#include <iostream>
#endif
#include <boost/variant.hpp>
#include <Rcpp.h>

namespace Xv {

template<typename T1, typename T2>
void check_Xv(const T1& Dim, const T2& y) {
  if (Dim[1] != y.size()) throw std::invalid_argument("Inconsistent dimension.");
}

template<typename T1, typename T2>
void check_vX(const T1& Dim, const T2& x) {
  if (Dim[0] != x.size()) throw std::invalid_argument("Inconsistent dimension.");
}

template<typename ReturnType>
size_t clean_row(std::function<bool(int)>& checker, const int nrow, ReturnType& result) {
  int src = 0, dst = 0;
  std::size_t size = 0;
  for (int row = 0;row < nrow;row++) {
    if (checker(row)) {
      src++;
      continue;
    }
    size++;
    result[dst++] = result[src++];
  }
  return size;
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgCMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant<Rcpp::IntegerVector, std::nullptr_t>& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
  ) {
  int
    *_i = INTEGER(X.slot("i")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  typedef std::function<bool(int)> Checker;
  Checker checker = [&foldid, &target, &is_exclude](int row) {
    return (boost::get<Rcpp::IntegerVector>(foldid)[row] == target) == is_exclude;
  };
  if (target == 0) {
    if (is_exclude) {
      checker = [](int row) {
        return false;
      };
    } else {
      checker = [](int row) {
        return true;
      };
    }
  }
  for(int col = 0;col < _Dim[1];col++) {
    for(int index = _p[col];index < _p[col + 1];index++) {
      int row = _i[index];
      if (checker(row)) continue;
      double value = _x[index];
      result[row] += v[col] * value;
    }
  }
  if (target == 0) return _Dim[0];
  return clean_row(checker, _Dim[0], result);
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgTMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant<Rcpp::IntegerVector, std::nullptr_t>& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
) {
  int
    *_i = INTEGER(X.slot("i")),
    *_j = INTEGER(X.slot("j")),
    *_Dim = INTEGER(X.slot("Dim")),
    _n = Rf_length(X.slot("i"));
  double *_x = REAL(X.slot("x"));
  typedef std::function<bool(int)> Checker;
  Checker checker = [&foldid, &target, &is_exclude](int row) {
    return (boost::get<Rcpp::IntegerVector>(foldid)[row] == target) == is_exclude;
  };
  if (target == 0) {
    if (is_exclude) {
      checker = [](int row) {
        return false;
      };
    } else {
      checker = [](int row) {
        return true;
      };
    }
  }
  for(int index = 0;index < _n;index++) {
    int row = _i[index];
    if (checker(row)) continue;
    int col = _j[index];
    double value = _x[index];
    result[row] += v[col] * value;
  }
  if (target == 0) return _Dim[0];
  return clean_row(checker, _Dim[0], result);
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgRMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant<Rcpp::IntegerVector, std::nullptr_t>& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
) {
  int
    *_j = INTEGER(X.slot("j")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  typedef std::function<bool(int)> Checker;
  Checker checker = [&foldid, &target, &is_exclude](int row) {
    return (boost::get<Rcpp::IntegerVector>(foldid)[row] == target) == is_exclude;
  };
  if (target == 0) {
    if (is_exclude) {
      checker = [](int row) {
        return false;
      };
    } else {
      checker = [](int row) {
        return true;
      };
    }
  }
  for(int row = 0;row < _Dim[0];row++) {
    if (checker(row)) continue;
    for(int index = _p[row];index < _p[row + 1];index++) {
      int col = _j[index];
      double value = _x[index];
      result[row] += v[col] * value;
    }
  }
  if (target == 0) return _Dim[0];
  return clean_row(checker, _Dim[0], result);
}

}

#endif // __XV_FOLDED_H__