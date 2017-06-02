#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
  
}

#'@importFrom methods setMethod
#'@importClassesFrom Matrix dgCMatrix
#'@export
setMethod("%*%", signature(x = "dgCMatrix", y = "numeric"), Xv_dgCMatrix_numeric)
