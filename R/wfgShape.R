source("wfgShapes.R")

#' wfgShape - used by wfgEval to apply a shape to part of the objective-space vector
#'
#' @param x \cr
#'   The point at which to evaluate the shape function. It has dimension of search-space.
#' @param current \cr
#'   The current shape function specified by the user
#' @param start.index
#' @param apply.length \cr
#'   Specifying to which entries to apply the shape to.
#' @param params \cr
#'   List of parameters to this shape
#' @return The vector x to which the shape has been applied. Those entries that were changed now have their final objective value, but more shapes can follow for the later indices.
#' @export
wfgShape = function(x, current, start.index, apply.length, params) {
  # x: the vector
  # current: a function
  # start.index/apply.length: to which entries to apply
  M = length(x)
  if (start.index+apply.length-1 > length(x)) stop("starting at ", start.index, " and applylength is ", apply.length, " but target has only length ", M)

  args = NA # scope
  if (length(params)>0) {
    args = c(NA, as.list(params)) # these 2 lines are to prepend x without flattening it
    args[[1]] = x[-M]
  } else {
    args = list(NA)
    args[[1]] = x[-M]
  }

  if (wfg.verbose) cat("calling ", attr(current, "name"), " with\n")
  if (wfg.verbose) str(args)
  shaped = do.call(current, args)
  # normally take the same entries, ie if target-vector's entry 2 should be shaped then take shaped's entry 2.
  # special case: mixed and disc only have one entry, take that regardless of where it should be placed in the target.
  
  if (identical(current, wfgMixed) || identical(current, wfgDisc)) { # compat
    if (apply.length!=1) stop("applylength of mixed or disc shapes has to be 1")
    x[start.index] = shaped[1]
  } else { # normal case    
    for (i in start.index:(start.index+apply.length-1) ) {
      if (wfg.verbose) cat("shaping index ", i, "\n")
      x[i] = shaped[i]
    }
  }
  if (wfg.verbose) cat("after this shaping:\n")
  if (wfg.verbose) str(x)
  return(x)
}
