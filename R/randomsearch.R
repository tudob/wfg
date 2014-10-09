# source("R/wfgUtil.R")

#' @rdname randomsearch
#' @export
randomdirection = function(dim) { # using Marsaglia's method
  x = rnorm(dim)
  r = sqrt(sum(x^2))
  return (x/r)
}
# example:
# m = matrix(NA, nrow=100, ncol=2)
# for (i in 1:100) {
#    m[i, ] = randomdirection(2)
# }
# plot(m)

#' random search
#'
#' An example algorithm used alongside the WFG package with the goal to be as simple as possible while still resembling better algorithms in that it is probabilistic, population-based and iterative (Rastrigin 1963).\cr\cr
#' It follows the steps:\cr
#' - Initialize individuals by uniform distribution in the search-space. \cr
#' - In each iteration try to improve each individual using a random direction and stepsize. \cr\cr
#' The random direction is chosen with Marsaglia's method (1972). \cr
#' The stepsize is chosen by an exponential distribution, preferring small steps. \cr
#'
#' @param func \cr
#'   The function to optimize
#' @param evals \cr
#'   The number of evaluations allowed. Will be divided into sqrt(evals) individuals and generations (heuristic).
#' @param inDim \cr
#'   The number of search-space dimensions.
#' @return The final points and their objective-values.
#' @export
randomsearch = function(func, evals=1000, inDim=5) {
  # cat("randomsearch with ", evals, " evaluations\n")
  points = floor(sqrt(evals)) # heuristic
  outDim = 2

  pars = matrix(rep(NA, points*inDim), nrow=points)
  values = matrix(rep(NA, points*outDim), nrow=points)

  # init
  for(i in 1:points) {
    pars[i, ] = runif(inDim, 0, 1)
    values[i, ] = func(pars[i, ])
  }

  # iter
  evalsRemaining = evals - points
  for (i in 0:(evalsRemaining-1)) {
    where = i%%points+1
    stepsize = rexp(inDim, rate= evals/points ) # random stepsize heuristic. 
                          # this is such that number of generations times average stepsize is 1.
    dir = randomdirection(inDim)
    new.par = to01( pars[where, ] + dir * stepsize)
    new.value = func(pars[where, ])
    if( all(new.value <= values[where, ])) {
      pars[where, ] = new.par
      values[where, ] = new.value
    } else { # worse
    }
  }
  mco = list(par=pars, value=values)
  return(mco)
}
