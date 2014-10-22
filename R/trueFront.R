#' trueFront
#'
#' A plot showing the true pareto front - making use of wfgEval's parameter true.front
#' @param name 
#'   Name under which to store a .png
#' @param spec 
#'   Specification of the test function to plot
#' @return nothing
#' @export
#' @examples
#' trueFront("truefrontLinear", c(sConvex))

trueFront = function(name, spec, removeDominated=TRUE) {
  png(paste(sep="", name, ".png"))
  set.seed(1)
  n = 1000
  m = matrix(NA, nrow=n, ncol=2)
  where = seq(0, 1, length.out=n)
  for(i in 1:n) {
    m[i, ] = wfgEval( c(where[i], where[i]), 2, true.front=TRUE, spec)
  }
  if(removeDominated) {
    m = nonDominated(m, 0)
  }
  m = m[order(m[, 1]), ]
  plot(m, pch=19, main="", xlab="", ylab="")
    # we would want to use type="L" and much fewer points, however we have to display a disconnected front.
  dev.off()
  cat("file saved\n")
  return (invisible(NULL))
}
# trueFront("truefrontLinear", c(sLinear))
