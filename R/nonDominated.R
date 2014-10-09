#' nonDominated
#'
#' nonDominated is similar to emoa::nonDominated_points() but it retains extra columns.
#' this allows us to know the search-space coordinates. 
#' this is similar to mco::paretoSet() however that depends on the optimization-algorithm being able to calculate the logical dominated column itself. this does not have any dependencies
#' @param matr \cr
#'   Matrix with an individual in each row. The meanings of columns are determined by numCoords:
#' @param numCoords \cr
#'   The first numCoords columns are usually the search-space coordinates of the individuals (other aspects can be stored as well), the remaining columns are the objective-space values of the individuals with which domination will be determined.
#' @return the matrix of non-dominated individuals
#' @export
#' @examples
#' this shows two values on the two axis and their two search-space coordinates as color and form
#' value1 = runif(20)
#' value2 = runif(20)
#' coord1 = 1:20
#' coord2 = 20:1
#' d = cbind(coord1, coord2, value1, value2)
#' d
#' nonDom = nonDominated(d, 2)
#' nonDom
#' plot(d[,3:4], col=d[, 1], pch=d[, 2], xlim=c(0,1), ylim=c(0,1))
#' plot(nonDom[,3:4], col=nonDom[, 1], pch=nonDom[, 2], xlim=c(0,1), ylim=c(0,1))

nonDominated = function(matr, numCoords) { # the first columns are coordinates, the remaining their values (on which to determine which are nonDom)
  numValues = ncol(matr)-numCoords
  l = list()
  for (i in 1:numValues) l[[i]] = matr[, numCoords+i]
  ord = do.call(order, l)
  D = matr[ord, ]
  nonDom = D
  for (i in 1:(numValues-1) ) {
    nonDom = nonDom[which(!duplicated(cummin(nonDom[, numCoords+i+1]))), ]
  }
  return (nonDom)
}
