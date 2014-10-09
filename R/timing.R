#' timing
#'
#' The timing function is used alongside the WFG package. It estimates the number of evaluations per second.
#'
#' @param expr \cr
#'   The expression to be timed.
#' @param maxSeconds \cr
#'   The maximum number of seconds you want to wait for the averaging. There are heuristics such that normally maxSeconds is not reached.
#' @return number of evaluations per second
#' @export
#' @examples
#' # usage:
#' timing(runif(100*1000))
#'
#' # comparing the evaluation speed of wfgEval and wfgWrap on a 5->2 test function of 4 transformations and 1 shape.
#' spec4 = c(tPoly, tDecept, tMulti, tNonsep, sConvex)
#' f4 = wfgWrap(2, spec4 )
#' timing(f4(rep(0.1, 5)))
#' timing(wfgEval(rep(0.1, 5), 2, spec4 ) )

# # some verification:
# rand.size = 100*1000
# num.evals = timing(runif(rand.size))
# num.evals
# p = proc.time()[1]
# for (i in 1:num.evals) runif(rand.size) 
# duration = proc.time()[1] - p # this should approx. 1 sec.
# if ( abs(duration - 1) > 0.1 ) stop("timing function inaccurate")

timing = function(expr, maxSeconds=4) { # returns average evals per second
  begin = proc.time()[1]
  totalEvals = 0
  currentTries = 1
  while (TRUE) {
    currentTime = proc.time()[1]
    if (currentTime > begin+maxSeconds) break;
    if (totalEvals>300 && currentTime-begin > 0.2) break;
    for (i in 1:currentTries) {
      eval(substitute(expr))
    }
    totalEvals = totalEvals + currentTries
    currentTries = ceiling(currentTries*1.2)
  }
  return ( totalEvals / as.numeric(proc.time()[1]-begin) )
}

