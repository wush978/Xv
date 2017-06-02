#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
}

evalqOnLoad({
#'@importFrom methods setMethod
#'@importClassesFrom Matrix dgCMatrix
setMethod("%*%", signature(x = "dgCMatrix", y = "numeric"), Xv:::Xv_dgCMatrix_numeric)
setMethod("%*%", signature(x = "numeric", y = "dgCMatrix"), Xv:::vX_numeric_dgCMatrix)
})
