# utility functions for the WFG implementation

# wfg.verbose = TRUE # flag: to show debug output
wfg.verbose = FALSE

to01 = function(x) { # (new) clamp vector x to 0-1-interval
  x = x-x*(x>1)+(x>1)
  x = x-x*(x<0)
  return (x)
}
# to01(c(1.1, -0.2))

dominates = function(x, y) {
  return ( all(x<=y)&&any(x<y) )
}

should01 = function(x) {
  if ( any(x>1.0) | any(x<0.0) ) stop(paste("should be between 0 and 1. is: ", x))
  return (x)
}
isNumOrNA = function(x) {
  if(is.function(x)) return (FALSE) # is.na would show warning
  return (is.na(x) || is.numeric(x))
}

shouldError = function(fct, ...) {
  erred = FALSE
  tryCatch( do.call(fct, list(...)), error = function(e) { erred <<- TRUE })
  if(!erred) stop("should have thrown an error")
}

shouldValues = function(vec) {
  if (is.null(vec)) stop("should not be null")
  if (any(is.na(vec))) stop("should contain no NA's")
  # return (vec)
  return (invisible(NULL))
}

rankMatrix = function(mat) {
  nr = nrow(mat)
  nc = ncol(mat)
  return ( matrix(rank(as.vector(mat)), nrow=nr, ncol=nc) )
}

#' parseParams - utility function for WFG
#' this is only exported for the tests
#'
#' @param strList \cr
#'   .
#' @param i \cr
#'   .
#' @return nothing
#' @export

parseParams = function(strList, i) {
  # i is the index of the current function. 
  # find the next function (or end):
  beyond = i+1
  len = length(strList)
  while (beyond<=len) {
    if(is.function(strList[[beyond]])) {
      break
    } else {
      beyond = beyond + 1
    }
  }
  if( i+1 > beyond-1 ) return (list())
  return (strList[(i+1):(beyond-1)])
}

