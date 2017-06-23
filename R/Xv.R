#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
}

#'@importFrom methods setMethod representation setClass signature setValidity setGeneric
#'@importClassesFrom Matrix dgCMatrix
#'@importClassesFrom Matrix dgTMatrix
#'@importClassesFrom Matrix dgRMatrix
#'@export folded.Xv folded.vX
evalqOnLoad({
  local({
    configureS4Method <- function(name, m.name) {
      force(name)
      force(m.name)
      setMethod("%*%", signature(x = m.name, y = "numeric"), get(sprintf("Xv_%s_numeric", m.name)))
      setMethod("%*%", signature(x = "numeric", y = m.name), get(sprintf("vX_numeric_%s", m.name)))
      getf <- function(fname) {
        force(fname)
        function(X, v, foldid, target, is_exclude) {
          get(fname)(X@m, v, foldid, target, is_exclude)
        }
      }
      setMethod(
        sprintf("folded.%s", name),
        signature(X = "Folded.dMatrix", v = "numeric", foldid = "integer", target = "integer", is_exclude = "logical"),
        getf(sprintf("folded.%s", name))
      )
      setMethod(
        sprintf("folded.%s", name), 
        signature(X = m.name, v = "numeric", foldid = "integer", target = "integer", is_exclude = "logical"),
        get(sprintf("%s_%s_numeric_folded", name, m.name))
      )
    }
    setClass(
      "Folded.dMatrix", 
      representation = representation(
        m = structure("dMatrix", package = "Matrix"),
        foldid = "integer",
        foldid.count = "integer" 
      )
    )
    setValidity("Folded.dMatrix", function(object) {
      stopifnot(length(object@foldid) == nrow(object@m))
      stopifnot(sum(object@foldid.count) == nrow(object@m))
      stopifnot(all(foldid > 0))
    })
    setMethod(
      "initialize",
      signature(.Object = "Folded.dMatrix"),
      function(.Object, m, foldid) {
        .Object@m <- m
        .Object@foldid <- foldid
        tb <- table(foldid)
        .Object@foldid.count <- as.integer(tb[paste(seq_len(length(unique(foldid))))])
        validObject(.Object)
        .Object
      }
    )

    setGeneric("folded.Xv", function(X, v, foldid, target, is_exclude) {
      standardGeneric("folded.Xv")
    })
    setGeneric("folded.vX", function(X, v, foldid, target, is_exclude) {
      standardGeneric("folded.vX")
    })
    for(name in c("Xv", "vX")) {
      for(m.name in c("dgCMatrix", "dgTMatrix", "dgRMatrix")) {
        configureS4Method(name, m.name)
      }
    }
  })
})

#'@name folded.Xv
#'@title Folded Matrix-Vector Multiplication
#'@param X Matrix.
#'@param v numeric vector.
#'@param foldid integer vector. Specifying the foldid of each rows.
#'@param target integer value. The target fold id
#'@param is_exclude logical value. Whether to exclude those rows belong to the target fold or not.
NULL