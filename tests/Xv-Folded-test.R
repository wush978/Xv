library(Xv)
source("R/init.R")

foldid <- rep(1:5, each = nrow(m1) / 5)
foldid <- sample(foldid, length(foldid))

fm.all <- lapply(m.all, function(m) {
  new("Folded.dMatrix", m, foldid)
})

# check result in R
cat("Checking Xv\n")
for(i in seq_along(m.all)) {
  m <- m.all[[i]]
  fm <- fm.all[[i]]
  cat(sprintf("matrix class: %s\n", class(m)))
  for(fold in 1:5) {
    cat(sprintf("fold: %d\n", fold))
    r1 <- m[fold == foldid,] %*% x1.1
    r2 <- folded.Xv(fm, x1.1, foldid, target = fold, is_exclude = FALSE)
    stopifnot(isTRUE(all.equal(r1, r2)))
    r1 <- m[fold != foldid,] %*% x1.1
    r2 <- folded.Xv(fm, x1.1, foldid, target = fold, is_exclude = TRUE)
    stopifnot(isTRUE(all.equal(r1, r2)))
  }
}

cat("Checking vX\n")
for(i in seq_along(m.all)) {
  m <- m.all[[i]]
  fm <- fm.all[[i]]
  cat(sprintf("matrix class: %s\n", class(m)))
  for(fold in 1:5) {
    cat(sprintf("fold: %d\n", fold))
    r1 <- x1.2[fold == foldid] %*% m[fold == foldid,] 
    r2 <- folded.vX(fm, x1.2[fold == foldid], foldid, target = fold, is_exclude = FALSE)
    stopifnot(isTRUE(all.equal(r1, r2)))
    r1 <- x1.2[fold != foldid] %*% m[fold != foldid,] 
    r2 <- folded.vX(fm, x1.2[fold != foldid], foldid, target = fold, is_exclude = TRUE)
    stopifnot(isTRUE(all.equal(r1, r2)))
  }
}
