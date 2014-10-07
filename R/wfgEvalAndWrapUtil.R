#' Utility functions for wfgEval
#' @export
checkTrafoComplete = function(target.index) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    stop("the last transformation specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
  }
  return (invisible(NULL))
}
#' @rdname checkTrafoComplete
#' @export
checkShapeComplete = function(target.index, current.shape) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    if ( identical(current.shape, wfgMixed) || identical(current.shape, wfgDisc) ) {
      stop("mixed and disconnected shapes always have applylength 1 but then this specification does not cover the remaining entries of the target")
    } else {
      stop("the last shape specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
    }
  }
  return (invisible(NULL))
}
#' @rdname checkTrafoComplete
#' @export
tToX = function(t, M, A) { # the t-to-x transition from the wfg-paper. this is applied after all transformations before all shapes
    # A: vector of {0, 1} degeneracy constants. for each that is 0 the p-front will have one less dimension
  # calc x from t:
  x = rep(NA, M)
  if (1<=M-1) for (i in 1:(M-1)) {
    x[i] = max(t[M], A[i])*(t[i]-0.5)+0.5
  }
  x[M]=t[M]
  return (x)
}
