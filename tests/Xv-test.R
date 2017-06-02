library(Xv)
library(Matrix)
m <- sparse.model.matrix(~ ., iris)
stopifnot(class(m) == "dgCMatrix")
set.seed(1)
x <- rnorm(ncol(m))
m %*% x
