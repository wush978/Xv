library(Xv)
source("R/init.R")

foldid <- rep(1:5, each = nrow(m1) / 5)
foldid <- sample(foldid, length(foldid))

fm.all <- lapply(m.all, function(m) {
  new("Folded.dMatrix", m, foldid)
})

for(i in seq_along(m.all)) {
  m <- m.all[[i]]
  fm <- fm.all[[i]]
  for(fold in 1:5) {
    cat(sprintf("%s fold: %d \n", class(m), fold))
    cat("r1\n")
    r1 <- m[fold == foldid,] %*% x1.1
    cat("r2\n")
    r2 <- folded.Xv(fm, x1.1, foldid, target = fold, is_exclude = FALSE)
    stopifnot(isTRUE(all.equal(r1, r2)))
  }
}
