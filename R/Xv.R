#'@useDynLib Xv, .registration=TRUE
#'@importFrom Rcpp evalCpp
.onLoad <- function(libname, pkgname) {
}

#'@importFrom methods setMethod representation
#'@importClassesFrom Matrix dgCMatrix
#'@importClassesFrom Matrix dgTMatrix
#'@importClassesFrom Matrix dgRMatrix
evalqOnLoad({
  
  for(name in c("dgCMatrix", "dgTMatrix", "dgRMatrix")) {
    setMethod("%*%", signature(x = name, y = "numeric"), get(sprintf("Xv_%s_numeric", name)))
    setMethod("%*%", signature(x = "numeric", y = name), get(sprintf("vX_numeric_%s", name)))
  }
  rm(name)
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

  
  for(name in c("Xv", "vX")) {
    fname <- sprintf("folded.%s", name)
    force(fname)
    getStandardGeneric <- function(fname) {
      force(fname)
      function(...) standardGeneric(fname)
    }
    #'@export folded.Xv folded.vX
    setGeneric(fname, function(X, v, foldid, target, is.exclude) {
      f <- force(getStandardGeneric(fname))
      f(X, v, foldid, target, is.exclude)
    })
    rm(getStandardGeneric)
    get.fname <- function(fname) {
      force(fname)
      get(fname)
    }
  
    for(m.name in c("dgCMatrix", "dgTMatrix", "dgRMatrix")) {
      getInternalFunction <- function(fname) {
        force(fname)
        function(X, v, foldid, target, is.exclude) {
          cat("Calling %s", fname)
          invisible(NULL)
        }
      }
      assign(
        sprintf("%s_%s_numeric_folded", name, m.name),
        value = getInternalFunction(sprintf("%s_%s_numeric_folded", name, m.name))
      )
      setMethod(
        fname,
        signature(X = m.name, v = "numeric", foldid = "integer", target = "integer", is.exclude = "logical"),
        get(sprintf("%s_%s_numeric_folded", name, m.name))
      )
    }
    rm(getInternalFunction)

    
    setMethod(
      fname,
      signature(X = "Folded.dMatrix", v = "numeric", foldid = "integer", target = "integer", is.exclude = "logical"),
      function(X, v, foldid, target, is.exclude) {
        get.fname(fname)(X@m, v, foldid, target, is.exclude)
      }
    )

    setMethod(
      fname,
      signature(X = "Folded.dMatrix", v = "numeric", foldid = "missing", target = "integer", is.exclude = "logical"),
      function(X, v, target, is.exclude) {
        get.fname(fname)(X@m, v, X@foldid, target, is.exclude)
      }
    )
    
    
  }
  rm(name, fname)
    

})
