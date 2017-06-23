
if (Sys.info()["sysname"] == "Darwin") {
  Rcpp::registerPlugin("openmp", Rcpp::Rcpp.plugin.maker())
  folds <- 1
} else {
  folds <- 1:2
}


# check C API

for(path in dir("cpp", pattern = "*.cpp$", full.names = TRUE)) {
  Rcpp::sourceCpp(path)
}
