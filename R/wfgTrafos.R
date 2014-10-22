# Transformations
# the function-names are those in the wfg-paper plus prefix wfg
# in the spec the user uses only the part after the "_" (no b/s/r-kind)

# conventions used:
# a parameter-default of =NA shows that there is a default (which will be calculated on demand) as opposed to a required value
# function naming here is as in wfg-paper plus prefix wfg (but tNone does not exist)

# source("R/wfgUtil.R")

#' WFG Transformations
#'
#' tNone is a transformation that changes nothing. It is used to move the entries cursor along to change later entries.\cr
#' tPoly is the polynomial bias transformation.\cr
#' tFlat creates a region in search space in which all points have the same objective values.\cr
#' tParam is the parameter-dependent transformation.\cr
#' tLinear creates a linear shift of the true optimum.\cr
#' tDecept creates regions in the search space that have a sub-optimal value but larger area.\cr
#' tMulti creates many local optima.\cr
#' tSum creates a dependence between different search-space entries.\cr
#' tNonsep creates a dependence between objectives.\cr
#'
#' @param y 
#'   The value of the search space entry to which to apply this to.
#' \cr
#' @param alpha 
#'   tPoly: alpha>1 biases toward 0, <1 biases toward 1.
#' \cr
#' @param value 
#'   tFlat: The value of the flat region. The adjacent regions interpolate to this value.
#' @param from 
#' @param to 
#'   tFlat: The region of the search space that is flat, same for every dimension.
#' \cr

#' @param y.prime 
#'   tParam: Is set by the system, it does not occur in the parameters the user writes in the specification
#' @param factor 
#'   tParam: Determines the slope of the ramp.
#' @param starter 
#' @param ender 
#'   tParam: The influence region.

#' \cr
#' @param zero.loc 
#'   tLinear: The location of the true optimum.
#' \cr

#' @param opti.loc 
#'   tDecept: The location of the true optimum.
#' @param aperture 
#'   tDecept: The size of the opening around the true optimum.
#' @param deceptive.value 
#'   tDecept: The value of the sub-optimal areas.
#' \cr

#' @param opti.loc 
#'   tMulti: The location of the true optimum.
#' @param num.minima 
#'   tMulti: The number of the local optima.
#' @param hill.size 
#'   tMulti: The size of the hills between the local optima.
#' \cr

#' @param i 
#' @param k 
#' @param M 
#'   tSum: Are set by the system, they do not occur in the parameters the user writes in the specification
#' @param from 
#' @param to 
#'   tSum: The entries which are made dependent on eachother.
#' @param weights 
#'   tSum: Optional for the weighted sum.
#' \cr

#' @param degree 
#'   tNonsep: Degree of nonseparability.
#' \cr

#' @return The modified value.
#' @export
wfgTrafos = function() {} # placeholder

#' @rdname wfgTrafos
#' @export
tNone = function(y) { return(y) }
attr(tNone, "type") = "wfgTrafo"
attr(tNone, "name") = "tNone"

#' @rdname wfgTrafos
#' @export
tPoly = function(y, alpha=0.02) { # alpha >1~<1 bias toward 0~1 (1.0 would be no change). default taken from wfg9 example
  if (alpha<=0) stop("alpha has to be greater than 0")
  res = y^alpha
  return(to01(res))
}
attr(tPoly, "type") = "wfgTrafo"
attr(tPoly, "name") = "tPoly"

#' @rdname wfgTrafos
#' @export
tFlat = function(y, value=0.8, from=0.75, to=0.85) { # defaults taken from wfg1 example
  if (wfg.verbose) cat("flat ", value, from, to, "\n")
  if (from>=to) stop("flat region should have: from < to")
  if (value<0 | value>1) stop("'value' should be between 0 and 1")
  if (from<0 | from>1) stop("'from' should be between 0 and 1")
  if (to<0 | to>1) stop("'to' should be between 0 and 1")
  # the following are difficult in practice, maybe disallow 0 and 1 alltogether? ie require 0<from<to<1
  if (from==0 & !( value==0 & to!=1)) stop("disallowed combination of parameter-values. from=0 requires value=0 and to!=1")
  if (to==1 & !(value==1 & from!=0)) stop("disallowed combination of parameter-values. to=1 requires value=1 and from!=0")
  A = value
  B = from
  C = to
  res = A + min(0, floor(y-B)) * A*(B-y)/B - min(0, floor(C-y)) * (1-A)*(y-C)/(1-C)
  return(to01(res))
}
attr(tFlat, "type") = "wfgTrafo"
attr(tFlat, "name") = "tFlat"

#' @rdname wfgTrafos
#' @export
tParam = function(y, y.prime=NA, factor=0.98/49.98, starter=0.02, ender=50) { # defaults taken from paper wfg7,8,9  (c++: 0.5, 2, 10)
                                # y.prime's value is set in wfgTrafo()
  A = factor
  B = starter
  C = ender
  if (A<=0 | A>=1) stop("A should be such that 0<A<1")
  if (B<=0) stop("B should be such that 0<B")
  if (C<=B) stop("B, C should be such that B<C")
  # for u() this implementation uses the identity
  v = function(y.prime) {
    A - (1-2*y.prime) * abs( floor(0.5-y.prime) + A )
  }
  res = y^( B+(C-B)*v(y.prime) )
  return(to01(res))
}
attr(tParam, "type") = "wfgTrafo"
attr(tParam, "name") = "tParam"

#' @rdname wfgTrafos
#' @export
tLinear = function(y, zero.loc=0.35) { # default taken from wfg1 example
  A = zero.loc
  if (A<=0 || A>=1) stop("zero-location A should be within (0, 1)")
  res = abs(y-A)/abs( floor(A-y)+A )
  return(to01(res))
}
attr(tLinear, "type") = "wfgTrafo"
attr(tLinear, "name") = "tLinear"

#' @rdname wfgTrafos
#' @export
tDecept = function(y, opti.loc=0.35, aperture=0.001, deceptive.value=0.05) { 
  # always optimumValue==0 and 2 deceptive points.
  # opti.loc: the location of the true optimum
  # aperture: size around the true optimum. (defaults taken from wfg9 example)
  # deceptive.value: the value of the local minimum of the 2 deceptive points
  A = opti.loc
  B = aperture
  C = deceptive.value
  if (A<=0 | A>=1) stop("optimum-location should be between 0 and 1 both excluding")
  if (B<=0) stop("aperture around the true optimum should be >0")
  if (B>0.25) stop("aperture around the true optimum should be much smaller than 1")
  if (C<=0) stop("deceptive.value should be >0")
  if (C>0.5) stop("deceptive.value should be much smaller than 1")
  if (A-B<=0) stop("this combination of optimum-location and aperture around the true optimum does not fit in the (0, 1) interval")
  if (A+B>=1) stop("this combination of optimum-location and aperture around the true optimum does not fit in the (0, 1) interval")
  res1 = ( floor(y-A+B)*(1-C+(A-B)/B) ) / (A-B)
  res2 = ( floor(A+B-y)*(1-C+(1-A-B)/B) ) / (1-A-B)
  res =  1 + (abs(y-A)-B) * ( res1+res2+1/B)
  return(to01(res))
}
attr(tDecept, "type") = "wfgTrafo"
attr(tDecept, "name") = "tDecept"

#' @rdname wfgTrafos
#' @export
tMulti = function(y, num.minima=30, hill.size=95, opti.loc=0.35) { # defaults taken from wfg9 example
  A = num.minima
  B = hill.size
  C = opti.loc
  if (A<=0 | floor(A)!=A) stop("num.minima has to be a natural number")
  if (B<0) stop("hill.size have to be >=0")
  if ( (4*A+2)*pi < 4*B ) stop("hill.size is too large. num.minima and hill.size should be such that (4*num.minima+2)*pi >= 4*hillsize")
  if (C<=0 | C>=1) stop("optimum-location should be in the interval (0,1)")
  res1 = abs(y-C) / (2* (floor(C-y)+C) )
  res2 = (4*A+2)*pi*(0.5-res1)
  res = ( 1+cos(res2)+4*B*res1^2 ) / (B+2)
  return(to01(res))
}
attr(tMulti, "type") = "wfgTrafo"
attr(tMulti, "name") = "tMulti"

#' @rdname wfgTrafos
#' @export
tSum = function(y, i, k, M, from=NA, to=NA, weights=NA) { # i, k, M: for system only (dont specify it). defaults for from/to see paper wfg1 and wfgTransformation.R
  n = length(y)
  w = weights
  if(i!=M) {
    if (is.na(from)) from = (i-1)*k/(M-1)+1   # defaults from wfg1
    if (is.na(to)) to = i*k/(M-1)
  } else {
    if (is.na(from)) from = k+1   # defaults from wfg1
    if (is.na(to)) to = n
  }
  if (any(is.na(w)) && !all(is.na(w))) stop("some weights are NA. Either none or all have to be defined.")
  if (all(is.na(w))) { # default from wfg1
    if(i!=M) {
      w = 2*(  ((i-1)*k/(M-1)+1) : (i*k/(M-1))  )
    } else {
      w = 2*( (k+1):n )
    }
    #alternative? w = seq(0.5, 1.5, length.out=to-from+1) # is this a useful default? (all 1-weights would not be)
  }
  y2 = y[from:to]
  if (all(is.na(w))) w = rep(1, length(y2))
  if (length(y2)!=length(w)) {
    if (length(y2)%%length(w)!=0) stop( paste("weights should have length ", length(y2), "or", length(y2),"should be a multiple" ))
    if (wfg.verbose) cat("note the weights", w, "will be extended to length ", to-from+1, "\n")
  }
  # the wfg-paper requires that all weights be >0
  if (any(w<0)) stop("weights should all be >=0")
  if (sum(w)==0) stop("at least one weight should be >0")
  res = sum(w*y2)/sum(w)
  return(to01(res))
}
attr(tSum, "type") = "wfgTrafo"
attr(tSum, "name") = "tSum"

#' @rdname wfgTrafos
#' @export
tNonsep = function(y, degree=NA) { # degree of nonseparability. degree's default is computed in wfgTransformation()
  A = degree

  # the best default degree-of-non-separability is the maximum: length(y), it obeys the constraint
  if (is.na(A)) A = length(y)

  if (A<=0 | floor(A)!=A) stop(paste("degree of nonseparability has to be an integer greater than 0 (it is ", A, ")"))
  accumOuter = 0
  for(j in 1:length(y)) {
    accum = y[j]
    if (0<=A-2) for(i in 0:(A-2)) {
      accum = accum + abs( y[j] - y[(1+j+i)%%length(y) +1] )
    }
    accumOuter = accumOuter + accum
  }
  temp = ceiling(A/2)
  res = accumOuter / ( length(y)/A*temp * (1+2*A-2*temp) )
  return(to01(res))
}
attr(tNonsep, "type") = "wfgTrafo"
attr(tNonsep, "name") = "tNonsep"

