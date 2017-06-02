if (isNamespaceLoaded("Xv")) unloadNamespace("Xv")
m1 <- Matrix::sparse.model.matrix(~ ., iris)
stopifnot(class(m1) == "dgCMatrix")
m2 <- as(m1, "dgTMatrix")
stopifnot(class(m2) == "dgTMatrix")
m.all <- list(m1, m2)
set.seed(1)
x1.1 <- rnorm(ncol(m1))
x1.2 <- rnorm(nrow(m1))
test.env <- new.env()

# check testing method

# define testing action
testXv <- function(name, m, x1, x2, action) {
  r.1 <- action(m, x1)
  r.2 <- action(x2, m)
  t.1 <- system.time(for(i in 1:1000) action(m, x1))
  t.2 <- system.time(for(i in 1:1000) action(x2, m))
  attr(r.1, "t") <- t.1
  attr(r.2, "t") <- t.2
  test.env[[sprintf("%s-Xv", name)]] <- r.1
  test.env[[sprintf("%s-vX", name)]] <- r.2
  invisible(NULL)
}

# Testing with Matrix
for(m in m.all) {
  .sm <- selectMethod("%*%", signature(x = class(m), y = class(x1.1)))
  stopifnot(.sm@defined@package == c("Matrix", "Matrix"))
  .sm <- selectMethod("%*%", signature(x = class(x1.2), y = class(m)))
  stopifnot(.sm@defined@package == c("Matrix", "Matrix"))
  testXv(sprintf("Matrix-%s-numeric", class(m)), m, x1.1, x1.2, function(x, y) (x %*% y)@x)
}

# Testing with Xv
loadNamespace("Xv")
for(m in m.all) {
  .sm <- selectMethod("%*%", signature(x = class(m), y = class(x1.1)))
  stopifnot(.sm@defined@package == c("Matrix", "methods"))
  .sm <- selectMethod("%*%", signature(x = class(x1.2), y = class(m)))
  stopifnot(.sm@defined@package == c("methods", "Matrix"))
  testXv(sprintf("Xv-%s-numeric", class(m)), m, x1.1, x1.2, function(x, y) x %*% y)
}

# Verify results
result.names <- grep("^Matrix", ls(test.env), value = TRUE)
for(name in result.names) {
  name.ref <- name
  name.test <- gsub("^Matrix", "Xv", name)
  stopifnot(name.test %in% ls(test.env))
  stopifnot(is.numeric(test.env[[name.ref]]))
  stopifnot(is.numeric(test.env[[name.test]]))
  stopifnot(test.env[[name.ref]] == test.env[[name.test]])
  stopifnot(attr(test.env[[name.test]], "t")[3] < attr(test.env[[name.ref]], "t")[3])
}
