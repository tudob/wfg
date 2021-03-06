# notes: 
# "decept": in wfg9 this is only applied to a certain k
# "multi": in wfg9 this is only applied to a certain k, here k=n

# source("R/wfgTransformations.R")

#' wfgTransformation - used by wfgEval to apply a transformation to part of the search-space vector
#'
#' @param M 
#'   The number of objectives
#' @param current 
#'   The current transformation function specified by the user
#' @param t_
#'   the vector
#' @param start.index
#'   Specifying to which entries to apply the transformation to.
#' @param apply.length 
#'   see start.index
#' @param n 
#'   The search-space dimension.
#' @param k 
#'   The number of position-dependent parameters.
#' @param params 
#'   List of parameters to this shape
#' @return The vector x to which the shape has been applied. Those entries that were changed now have their final objective value, but more shapes can follow for the later indices.
#' @export
wfgTransformation = function(M, current, t_, start.index, apply.length, n, k, params) { 
  # current: a function
  # t_: the vector
  # start.index/apply.length: to which entries to apply
  # n: in-dim
  # k: num pos-dep
  # params: list of parameter-numbers (or NAs) following the symbol
  # returns c(NA) if not a transformation -> main switches to shapes
  if (wfg.verbose) cat("trafo. start:", start.index, " apply.length:", apply.length, "\n")
  if (apply.length<=0) stop("number of entries (to which the transformation will be applied) has to be positive")
  if (start.index<=0) stop("err")
  
  for(i in start.index:(start.index+apply.length-1)) {
    # first handle 3 special cases. the else is the normal case for all other trafos
    if (identical(current, tParam)) {
      # special case because we cannot apply tSum first and then tParam because tParam uses the orig AND tSum's output.
            
      # calc tSum:
      # from = (i-1)*k/(M-1)+1
      # to = i*k/(M-1)
      if(i!=n) { # default from wfg7&9
        from = i+1
        to = n
      } else { # default from wfg8
        from = 1
        to = i-1
      }
      #if (from>n) { # do it like wfg8
      #  from = 1
      #  to = i-1
      #}
      if(wfg.verbose) cat("tSum from param\n")
      sum_res = tSum( t_[1:n], i, k, M, from, to, rep(1, to-from+1 ) )
      
      para = c(t_[i], sum_res, params)
      if (wfg.verbose) cat("(s)t: calling tParam with\n")
      if (wfg.verbose) str(para)
      t_[i] = do.call(tParam, para)

    } else if (identical(current, tSum)) { # sum needs the whole vector not just the i'th entry
      if (wfg.verbose) cat("(s)t: tSum\n")
      para = c(NA, NA, NA, NA, params) # NA to prepend t_ without flattening it
      para[[1]] = t_
      para[[2]] = i    # additional params needed by tSum's defaults-calculations
      para[[3]] = k
      para[[4]] = M
      if (length(para)!=length(params)+4) stop("t_ flattened")
      if(wfg.verbose) { cat("tSum with:\n"); print(para) }
      t_[i] = do.call(tSum, para)

    } else if (identical(current, tNonsep)) {
      # wfg9: reduces to M objectives (?)
      if (wfg.verbose) cat("(s)t: non-separable objective\n")
      if (length(params)>1) stop("nonseparable has only 1 (optional) parameter")
      para = NA
      if (length(params)==1) para = params[[1]]
      bounds = NA # scope
      
      if(i!=M) { # compare wfg6,9
        bounds = ((i-1)*k/(M-1)) : (i*k/(M-1))
      } else {
        bounds = (k+1):n
      }
      
      t_[i] = tNonsep( t_[ bounds ] , para )

    } else { # normal case for all other trafos
      para = c(t_[i], params)
      if (wfg.verbose) cat("(d)calling ", attr(current, "name")," with\n")
      if (wfg.verbose) str(para)
      t_[i] = do.call(current, para)
    }
  }
  return(t_)
}
