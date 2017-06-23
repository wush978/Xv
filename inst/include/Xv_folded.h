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


template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgCMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant<Rcpp::IntegerVector, nullptr_t>& foldid = nullptr,
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
  int src = 0, dst = 0;
  std::size_t size = 0;
  for (int row = 0;row < _Dim[0];row++) {
    if (checker(row)) {
      src++;
      continue;
    }
    size++;
    result[dst++] = result[src++];
  }
  return size;
}

}

#endif // __XV_FOLDED_H__