# Shapes:
# the function-names are those in the wfg-paper plus prefix wfg
# in the spec the user writes it without wfg:
#
# conventions used:
# *here* ie in individual shapes: x always has dim M-1 (not M). result is always M-dim.

# source("R/wfgUtil.R")

#' WFG Shapes
#'
#' sNone is a shape that changes nothing. It is used to move the entries cursor along to change later entries.\cr
#' sLinear is the linear pareto frontier. \cr
#' sConvex is the convex pareto frontier. \cr
#' sConcave is the concave pareto frontier. \cr
#' sMixed is a shape of the pareto frontier that has some convex and concave parts. \cr
#' sDisc is a shape of the pareto frontier that is not continuous.\cr
#'
#' @param x 
#'   The vector in objective space without its last entry.
#'
#' @param overall 
#'   The overall form of the pareto frontier, if it is >1 then it is more convex, if <1 then more concave.
#' @param num 
#'   Number of parts of the pareto frontier.
#' @param location 
#'   Where the discontinuities are. A larger value moves it to larger values of the first objective.

#' @return The modified vector.
#' @export
wfgShapes = function() {} # placeholder

#' @rdname wfgShapes
#' @export
sNone = function(x) { return (invisible(NULL)) }
attr(sNone, "type") = "wfgShape"
attr(sNone, "name") = "sNone"

#' @rdname wfgShapes
#' @export
sLinear = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(x)
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod(x[1:(M-m)]) * (1-x[M-m+1])
  res[M] = 1-x[1]
  return(to01(res))
}
attr(sLinear, "type") = "wfgShape"
attr(sLinear, "name") = "sLinear"

#' @rdname wfgShapes
#' @export
sConvex = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(1 - cos(x*pi/2))
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod( 1-cos( x[1:(M-m)]*pi/2) ) * (1 - sin( x[M-m+1]*pi/2 ) )
  res[M] = 1 - sin(x[1]*pi/2)
  return(to01(res))
}
attr(sConvex, "type") = "wfgShape"
attr(sConvex, "name") = "sConvex"

#' @rdname wfgShapes
#' @export
sConcave = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(sin(x*pi/2))
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod( sin( x[1:(M-m)]*pi/2) ) * cos( x[M-m+1]*pi/2 )
  res[M] = cos(x[1]*pi/2)
  return(to01(res))
}
attr(sConcave, "type") = "wfgShape"
attr(sConcave, "name") = "sConcave"

#' @rdname wfgShapes
#' @export
sMixed = function(x, overall=1.0, num=2) { # overall>1~<1 => convex~concave. num is number of convex/concave regions
  if (num%%1!=0) stop("number of convex/concave regions should be an integer")
  # only uses x[1] think correct because in 'disconnected' for beta they state x[1] explicitly
  if (overall<=0) stop("overall shape has to be >0")
  if (num<=0 | floor(num)!=num) stop("number of convex+concave parts has to be a natural number")
  if (wfg.verbose) cat("mixed with", overall, "and", num, "\n")
  A = num # assignments like these are to match the wfg-paper (while still having descriptive parameter-names in R)
  alpha = overall
  temp = 2*A*pi
  res = (1-x[1]- cos(temp*x[1] + pi/2)/temp) ^ alpha
  return(to01(res))
}
attr(sMixed, "type") = "wfgShape"
attr(sMixed, "name") = "sMixed"

#' @rdname wfgShapes
#' @export
sDisc = function(x, overall=1.0, num=2, location=1.0) {  #!!! overall>1~<1 => concave~convex. the opposite of the above! acc. to paper. paper correct?
  if (overall<=0) stop("overall shape has to be >0")
  if (location<=0) stop("location has to be >0")
  if (num<=0 | floor(num)!=num) stop ("number of disconnected regions has to be a natural number")
  A = num
  alpha = overall
  beta = location
  res = 1 - x[1]^alpha * cos( A * x[1]^beta * pi )^2 
  return(to01(res))
}
attr(sDisc, "type") = "wfgShape"
attr(sDisc, "name") = "sDisc"

