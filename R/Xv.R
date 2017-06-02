#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
}

evalqOnLoad({
#'@importFrom methods setMethod
#'@importClassesFrom Matrix dgCMatrix
setMethod("%*%", signature(x = "dgCMatrix", y = "numeric"), Xv:::Xv_dgCMatrix_numeric)
})
