#' withDominated 
#' 
#' withDominated is a visualization used alongside the wfg package. It shows the usual representation of objective space and pareto-frontier but complements it with the dominated individuals.\cr
#' baGraphicsWithDominated is a wrapper taking a filename for output and a specification for the test-function.
#'
#' @param func 
#'   The function to plot
#' @param spec 
#'   Specification of the function to plot
#' @param name
#'   Name of the .png file to generate
#' @return nothing
#' @export
#' @examples
#' withDominated(wfgWrap(2, c(sConvex)))
#' baGraphicsWithDominated("withDominatedTFlat", c(tFlat, from=0.4, to=0.9, sLinear))

withDominated = function(func) {
  set.seed(1)
  n = 1000
  m = matrix(NA, nrow=n, ncol=2)
  for(i in 1:n) {
    m[i, ] = func( runif(2) )
  }
  m = m[order(m[, 1]), ]
  plot(m, pch=3, main="", xlab="", ylab="")
  return (invisible(NULL))
}
#' @rdname withDominated
#' @export
baGraphicsWithDominated = function(name, spec) {
  png(paste(sep="", name, ".png"))
  withDominated(wfgWrap(2, spec))
  dev.off()
  return (invisible(NULL))
}

# withDominated(wfgWrap(2, c(sConvex)))

# baGraphicsWithDominated("withDominatedSLinear", c(sLinear))
# baGraphicsWithDominated("withDominatedSConvex", c(sConvex))
# baGraphicsWithDominated("withDominatedSConcave", c(sConcave))

# baGraphicsWithDominated("withDominatedSMixed", c(sMixed, sMixed))
# baGraphicsWithDominated("withDominatedSDisc", c(sDisc, sDisc))

# baGraphicsWithDominated("withDominatedTNothing", c(sLinear))
# baGraphicsWithDominated("withDominatedTPoly", c(tPoly, NA, 5, sLinear))
# baGraphicsWithDominated("withDominatedTFlat", c(tFlat, from=0.4, to=0.9, sLinear))
# baGraphicsWithDominated("withDominatedTParam", c(tParam, NA, 0.9, sLinear))
# baGraphicsWithDominated("withDominatedTLinear", c(tLinear, NA, 0.8, sLinear))

# baGraphicsWithDominated("withDominatedTDecept", c(tDecept, aperture = 0.1, sLinear))
# baGraphicsWithDominated("withDominatedTMulti", c(tMulti, hill.size=2, num.minima=100, sLinear))
# baGraphicsWithDominated("withDominatedTSum", c(tSum, sLinear))
# baGraphicsWithDominated("withDominatedTNonsep", c(tNonsep, sLinear))
