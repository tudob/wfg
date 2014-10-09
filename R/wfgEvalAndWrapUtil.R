#' Utility functions for wfgEval and wfgWrap
#'
#' used only within wfgEval \cr\cr
#' checkTrafoComplete: different transformations can be applied throughout the vector - this can be done multiple times, each time the vector wraps around (from last index to first) - the last transformation overall has to end at the last entry of the vector, this function checks that condition.\cr\cr
#' checkShapeComplete: similar to checkTrafoComplete (however shapes dont wrap around).\cr\cr
#' tToX: transformations are applied to the search-space vector, shapes to the objective-space vector. tToX does the transformation of the first to the second and also integrates the degeneracy factor A. \cr\cr

#' @export
wfgEvalAndWrapUtil = function() {} # placeholder

#' @rdname wfgEvalAndWrapUtil
#' @export
checkTrafoComplete = function(target.index) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    stop("the last transformation specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
  }
  return (invisible(NULL))
}
#' @rdname wfgEvalAndWrapUtil
#' @export
checkShapeComplete = function(target.index, current.shape) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    if ( identical(current.shape, sMixed) || identical(current.shape, sDisc) ) {
      stop("mixed and disconnected shapes always have applylength 1 but then this specification does not cover the remaining entries of the target")
    } else {
      stop("the last shape specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
    }
  }
  return (invisible(NULL))
}
#' @rdname wfgEvalAndWrapUtil
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
