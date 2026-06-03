#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib ripley, .registration = TRUE
## usethis namespace: end
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("ripley", libpath)
}
