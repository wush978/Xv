#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
}

#'@importFrom methods setMethod
#'@importClassesFrom Matrix dgCMatrix
#'@importClassesFrom Matrix dgTMatrix
#'@importClassesFrom Matrix dgRMatrix
evalqOnLoad({
  for(name in c("dgCMatrix", "dgTMatrix", "dgRMatrix")) {
    setMethod("%*%", signature(x = name, y = "numeric"), get(sprintf("Xv_%s_numeric", name)))
    setMethod("%*%", signature(x = "numeric", y = name), get(sprintf("vX_numeric_%s", name)))
  }
  rm(name)
})
