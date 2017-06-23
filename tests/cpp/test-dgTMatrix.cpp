//[[Rcpp::plugins(cpp14)]]
//[[Rcpp::plugins(openmp)]]
//[[Rcpp::depends(Xv, BH)]]

#ifdef _OPENMP
#include <omp.h>
#else
static int num_thread = 0;

static void omp_set_num_threads(int num) {
  num_thread = num;
}

static int omp_get_thread_num(void) {
  return 0;
}
#endif
#include <Xv.h>
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
SEXP testXv(S4 m, NumericVector x, IntegerVector foldid) {
  std::vector< std::vector<double> > result;
  int max_fold = *std::max_element(foldid.begin(), foldid.end());
  omp_set_num_threads(max_fold);
  result.resize(max_fold);
  const IntegerVector _Dim(m.slot("Dim"));
#pragma omp parallel
{
  std::vector<double>& result_element(result[omp_get_thread_num()]);
  result_element.resize(_Dim[0], 0.0);
  std::size_t size = Xv::Xv_dgTMatrix_numeric_folded(m, x, result_element, foldid, omp_get_thread_num() + 1, false);
  result_element.resize(size);
}
return wrap(result);
}

//[[Rcpp::export]]
SEXP testvX(S4 m, NumericVector x, IntegerVector foldid) {
  std::vector< std::vector<double> > result;
  int max_fold = *std::max_element(foldid.begin(), foldid.end());
  omp_set_num_threads(max_fold);
  result.resize(max_fold);
  const IntegerVector _Dim(m.slot("Dim"));
#pragma omp parallel
{
  int target = omp_get_thread_num() + 1;
  bool is_exclude = false;
  std::vector<double>& result_element(result[omp_get_thread_num()]);
  result_element.resize(_Dim[1], 0.0);
  auto checker(Xv::create_checker(foldid, target, is_exclude));
  auto local_row_mapping(Xv::create_local_row_mapping(nullptr, target, checker, _Dim[0]));
  std::vector<double> local_x(x.size(), 0.0);
  for(int row = 0;row < _Dim[0];row++) {
    if (checker(row)) continue;
    local_x[local_row_mapping(row)] = x[row];
  }
  Xv::vX_dgTMatrix_numeric_folded(m, local_x, result_element, foldid, target, false, local_row_mapping);
}
return wrap(result);
}

/***R
library(methods)
  library(Matrix)
  m <- matrix(1:4, 2, 2)
  m <- as(m, "dgTMatrix")
  set.seed(1)
  foldid <- 1:2

x <- 1:ncol(m)
  r1 <- head(testXv(m, x, foldid), max(folds))
  r2 <- lapply(folds, function(fold) {
    result <- m[foldid == fold,] %*% x
    if (isS4(result)) result@x else as.vector(result)
  })
  stopifnot(isTRUE(all.equal(r1, r2)))
  
  x <- 1:nrow(m)
  r1 <- head(testvX(m, x, foldid), max(folds))
  r2 <- lapply(folds, function(fold) {
    result <- x[foldid == fold] %*% m[foldid == fold,]
    if (isS4(result)) result@x else as.vector(result)
  })
  stopifnot(isTRUE(all.equal(r1, r2)))
  
  */