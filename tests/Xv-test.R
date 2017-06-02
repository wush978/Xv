m1 <- Matrix::sparse.model.matrix(~ ., iris)
stopifnot(class(m1) == "dgCMatrix")
set.seed(1)
x1.1 <- rnorm(ncol(m1))
x1.2 <- rnorm(nrow(m1))


.sm <- selectMethod("%*%", signature(x = class(m1), y = class(x1.1)))
stopifnot(.sm@defined@package == c("Matrix", "Matrix"))
r1.1 <- (m1 %*% x1.1)@x
r1.2 <- (x1.2 %*% m1)@x
t1.1 <- system.time({
  for(i in 1:1000) {
    (m1 %*% x1.1)
  }
})
t1.2 <- system.time({
  for(i in 1:1000) {
    (x1.2 %*% m1)
  }
})
library(Xv)
.sm <- selectMethod("%*%", signature(x = class(m1), y = class(x1.1)))
stopifnot(.sm@defined@package == c("Matrix", "methods"))
r1.1 <- m1 %*% x1.1
stopifnot(class(r1.1) == "numeric")
# r1.2 <- x1.2 %*% m1
# stopifnot(class(r1.2) == "numeric")
