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

class Mapper {
  std::vector<int> mapper;
public:
  Mapper(std::function<bool(int)>& checker, const int nrow) : mapper(nrow, 0) {
    int target_row = 0;
    for(int row = 0;row < nrow;row++) {
      if (checker(row)) {
        mapper[row] = -1;
      } else {
        mapper[row] = target_row++;
      }
    }
  }
  
  ~Mapper() { }
  
  int operator()(int row) {
    return mapper[row];
  }
  
};

inline std::function<int(int)> create_row_mapping(
    std::function<bool(int)>& checker,
    const int nrow
) {
  return Mapper(checker, nrow);
}

class Checker {
  const Rcpp::IntegerVector& _foldid;
  const int _target;
  const bool _is_exclude;
public:
  Checker(
    const boost::variant<std::nullptr_t,Rcpp::IntegerVector>& foldid,
    const int target,
    const bool is_exclude
  ) : _foldid(boost::get<Rcpp::IntegerVector>(foldid)), _target(target), _is_exclude(is_exclude) 
  {
  }
  ~Checker() { }
  bool operator()(int row) {
    return (_foldid[row] == _target) == _is_exclude;
  }
};

inline std::function<bool(int)> create_checker(
  const boost::variant<std::nullptr_t,Rcpp::IntegerVector>& foldid,
  const int target,
  const bool is_exclude
) {
  if (target == 0) {
    if (is_exclude) {
      return [](int row) {
        return false;
      };
    } else {
      return [](int row) {
        return true;
      };
    }
  } else {
    return Checker(foldid, target, is_exclude);  
  }
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgCMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant<std::nullptr_t,Rcpp::IntegerVector>& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
  ) {
  int
    *_i = INTEGER(X.slot("i")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
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

inline std::function<int(int)> create_local_row_mapping(
  const boost::variant< std::nullptr_t, std::function<int(int)> >& row_mapping_container,
  const int target,
  std::function<bool(int)>& checker,
  const int nrow
) {
  std::function<int(int)> result = [](int row) {
    return row;
  };
  if (target > 0) {
    if (row_mapping_container.which() == 0) {
      result = create_row_mapping(checker, nrow);
    }
  }
  return result;
}

template<typename InputVecType, typename ReturnType>
std::size_t vX_dgCMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant< std::nullptr_t, Rcpp::IntegerVector >& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true,
    const boost::variant< std::nullptr_t, std::function<int(int)> >& row_mapping_container = nullptr
) {
  int
    *_i = INTEGER(X.slot("i")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
  std::function<int(int)> local_row_mapping(create_local_row_mapping(row_mapping_container, target, checker, _Dim[0]));
  const std::function<int(int)>& row_mapping(
    target > 0 & row_mapping_container.which() > 0 ?
    boost::get< std::function<int(int)> >(row_mapping_container) :
    local_row_mapping
  );
  for(int col = 0;col < _Dim[1];col++) {
    for(int index = _p[col];index < _p[col + 1];index++) {
      int row = _i[index];
      if (checker(row)) continue;
      double value = _x[index];
      result[col] += v[row_mapping(row)] * value;
    }
  }
  return _Dim[1];
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgTMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant< std::nullptr_t, Rcpp::IntegerVector >& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
) {
  int
    *_i = INTEGER(X.slot("i")),
    *_j = INTEGER(X.slot("j")),
    *_Dim = INTEGER(X.slot("Dim")),
    _n = Rf_length(X.slot("i"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
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
std::size_t vX_dgTMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant< std::nullptr_t, Rcpp::IntegerVector >& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true,
    const boost::variant< std::nullptr_t, std::function<int(int)> >& row_mapping_container = nullptr
) {
  int
    *_i = INTEGER(X.slot("i")),
    *_j = INTEGER(X.slot("j")),
    *_Dim = INTEGER(X.slot("Dim")),
    _n = Rf_length(X.slot("i"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
  std::function<int(int)> local_row_mapping(create_local_row_mapping(row_mapping_container, target, checker, _Dim[0]));
  const std::function<int(int)>& row_mapping(
    target > 0 & row_mapping_container.which() > 0 ?
    boost::get< std::function<int(int)> >(row_mapping_container) :
    local_row_mapping
  );
  for(int index = 0;index < _n;index++) {
    int row = _i[index];
    if (checker(row)) continue;
    int col = _j[index];
    double value = _x[index];
    result[col] += v[row_mapping(row)] * value;
  }
  return _Dim[1];
}

template<typename InputVecType, typename ReturnType>
std::size_t Xv_dgRMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant< std::nullptr_t, Rcpp::IntegerVector >& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true
) {
  int
    *_j = INTEGER(X.slot("j")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
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

template<typename InputVecType, typename ReturnType>
std::size_t vX_dgRMatrix_numeric_folded(
    const Rcpp::S4& X,
    InputVecType& v,
    ReturnType& result,
    const boost::variant< std::nullptr_t, Rcpp::IntegerVector >& foldid = nullptr,
    const int target = 0,
    const bool is_exclude = true,
    const boost::variant< std::nullptr_t, std::function<int(int)> >& row_mapping_container = nullptr
) {
  int
    *_j = INTEGER(X.slot("j")),
    *_p = INTEGER(X.slot("p")),
    *_Dim = INTEGER(X.slot("Dim"));
  double *_x = REAL(X.slot("x"));
  std::function<bool(int)> checker(create_checker(foldid, target, is_exclude));
  std::function<int(int)> local_row_mapping(create_local_row_mapping(row_mapping_container, target, checker, _Dim[0]));
  const std::function<int(int)>& row_mapping(
    target > 0 & row_mapping_container.which() > 0 ?
    boost::get< std::function<int(int)> >(row_mapping_container) :
    local_row_mapping
  );
  for(int row = 0;row < _Dim[0];row++) {
    if (checker(row)) continue;
    int mapped_row = row_mapping(row);
    for(int index = _p[row];index < _p[row + 1];index++) {
      int col = _j[index];
      double value = _x[index];
      result[col] += v[mapped_row] * value;
    }
  }
  return _Dim[1];
}


}

#endif // __XV_FOLDED_H__