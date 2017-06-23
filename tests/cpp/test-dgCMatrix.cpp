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
SEXP test(S4 m, NumericVector x, IntegerVector foldid) {
  std::vector< std::vector<double> > result;
  int max_fold = *std::max_element(foldid.begin(), foldid.end());
  omp_set_num_threads(max_fold);
  result.resize(max_fold);
  const IntegerVector _Dim(m.slot("Dim"));
#pragma omp parallel
  {
    std::vector<double>& result_element(result[omp_get_thread_num()]);
    result_element.resize(_Dim[0], 0.0);
    std::size_t size = Xv::Xv_dgCMatrix_numeric_folded(m, x, result_element, foldid, omp_get_thread_num() + 1, false);
    result_element.resize(size);
  }
  return wrap(result);
}

/***R
library(methods)
library(Matrix)
m <- sparse.model.matrix(~ ., iris)
x <- rnorm(ncol(m))
foldid <- sample(1:3, nrow(m), TRUE)
r1 <- head(test(m, x, foldid), folds)
r2 <- lapply(folds, function(fold) {
  result <- m[foldid == fold,] %*% x
  if (!is.numeric(result)) result@x else result
})
stopifnot(isTRUE(all.equal(r1, r2)))
*/