# source("R/nonDominated.R")
#  source("R/randomsearch.R") # this is only needed for the green dots

# TODO: 
# find out whether this kind of plot is considered good/bad. name?

# note pixmapRGB issues a warning: 'x' is NULL so the result will be NULL
#     however, pixmapRGB does not have a parameter 'x', so it has to be some problem internal to pixmapRGB

# generate rgb-bitmaps then use package pixmap to show the bitmaps
# install.packages("pixmap")
library(pixmap)
# ?pixmapRGB

# we can show the pareto-set as black pixels (better green but only if only red/blue are used)
# we use nsga2 here
# install.packages("mco")
# library(mco)

#' toPlot
#'
#' A visualization used alongside the wfg package. It shows for each point in the discretized search-space which objective-values will be realized by it (as color).\cr\cr
#' At the moment the first two search-space dimensions are selected (this should be parameterized in the future).\cr\cr
#' It is possible to show an RGB- or Red-Blue-plot (chosen because of common red-green blindness).\cr\cr
#' Red-Blue: Red shows a large value in the first objective, Blue shows a large value in the second objective.\cr
#' Green dots show the non-dominated individuals of a run of random-search, it is surprisingly instructive.\cr\cr
#' toPlot22 is a wrapper taking a wfg spec instead of an arbitrary function.\cr
#'
#' @param func \cr
#'   The function to be shown
#' @param ranked \cr
#'   Decides whether the colors are chosen based on ranked objective-values. This is recommended to distinguish colors but it does not allow interpretation of their absolute values.
#' @param inDim \cr
#'   Number of search-space dimensions; needed for the optimization algorithm (e.g. random search)
#' @return nothing
#' @export
#' @examples
#' toPlot(wfgWrap(2, c(tDecept, aperture = 0.25, sLinear) ) )
#' toPlot22( c(tFlat, from=0.4, to=0.9, sLinear) )
#' toPlot22( c(tMulti, hill.size=0, num.minima=100, sLinear) )
#' toPlot22( c(tNonsep, degree=10, sLinear) )

toPlot = function(func, ranked=TRUE, inDim=5, ...) {
  cx = 50
  cy = 50
  red = matrix(0, cx, cy)
  green = matrix(0, cx, cy) # green is not used when only 2 objectives (prefer blue because of red/green blindness)
  blue = matrix(0, cx, cy)
  xs = seq(0, 1, length.out=cx)
  ys = seq(0, 1, length.out=cy)
  
  # trial eval to determine number of objectives
  trial = func(c(xs[1], ys[1], 0, 0, 0))
  numObjs = length(trial)

  # evaluate all points. and fill rgb accordingly
  if(numObjs==1) { # all colors the same => grayscale   or if only red/blue-plot then violet
    for(ix in 1:cx) {
      for(iy in 1:cy ) {    
        objectives = func(c(xs[ix], ys[iy], 0, 0, 0))
        red[ix, iy] = green[ix, iy] = blue[ix, iy] = objectives[1]
      }
    }
  } else if(numObjs==2) { # red blue
    for(ix in 1:cx) {
      for(iy in 1:cy ) {    
        objectives = func(c(xs[ix], ys[iy], 0, 0, 0))
        red[ix, iy] = objectives[1]
        blue[ix, iy] = objectives[2]
      }
    }
  } else if(numObjs==3) { # red green blue
    for(ix in 1:cx) {
      for(iy in 1:cy ) {    
        objectives = func(c(xs[ix], ys[iy], 0, 0, 0))
        red[ix, iy] = objectives[1]
        green[ix, iy] = objectives[2] 
        blue[ix, iy] = objectives[3]
      }
    }
  } else {
    stop("more than 3 objectives not supported")
  }

  # scaling colors in the graphic:
  # (otherwise for some problems too few colors are distinguishable (e.g. everything seems solid red))
  if(ranked) {
    red = rankMatrix(red)
    green = rankMatrix(green)
    blue = rankMatrix(blue)
    red = red - min(red)
    green = green - min(green)
    blue = blue - min(blue)
  }

  # scale colors to [0, 1]
  redMin = min(red)
  redMax = max(red)
  blueMin = min(blue)
  blueMax = max(blue)
  greenMin = min(green)
  greenMax = max(green)
  # apply ranges -> colors [0, 1]
  if(redMax-redMin>0.0) red = (red-redMin)/(redMax-redMin)
  if(blueMax-blueMin>0.0) blue = (blue-blueMin)/(blueMax-blueMin)
  if(greenMax-greenMin>0.0) green = (green-greenMin)/(greenMax-greenMin)
  
  # evaluate algorithm
  # res = wrapNsga2(func)
  res = randomsearch(func, 1000, inDim)
  plot(res$value, ...) # plot the p-Front and dominated(!) points
  res = cbind(res$par, res$value)

  nonDom = nonDominated(res, inDim)
  values = nonDom[, (inDim+1):ncol(nonDom)]
  plot(values, ...) # plot the p-Front
  
  px = nonDom[, 1]
  px[px==1.0] = 0.9999 # fix. 1 would possibly result in invalid indexing below
  px = floor(px*cx) # real-coord -> pixel     # todo change if coords are not 0-1 anymore
  
  py = nonDom[, 2]
  py[py==1.0] = 0.9999 # fix. 1 would possibly result in invalid indexing below
  py = floor(py*cy)
  
  # draw nsga2 results into matrix
  # for 1 or 3 objectives set the matrices to the color that has max contrast with the original
  # for 2 objectives (r/b) green is used
  if(numObjs==2) {
    for(i in 1:length(px)) {
      red[px[i], py[i]+1] = 0
      green[px[i], py[i]+1] = 1
      blue[px[i], py[i]+1] = 0
    }
  } else {
    for(i in 1:length(px)) {
      red[px[i], py[i]+1] = 1 - round(red[px[i], py[i]+1])
      green[px[i], py[i]+1] = 1 - round(green[px[i], py[i]+1])
      blue[px[i], py[i]+1] = 1 - round(blue[px[i], py[i]+1])
    }
  }

  # plot the param-space. the optimized points were added as black into the matrix
  #cat("to pix r", red, "\n")
  #cat("to pix b", blue, "\n")
  if(any(is.na(red))) stop("red")
  if(any(is.na(green))) stop("green")
  if(any(is.na(blue))) stop("blue")
  #
  # for(ix in 5:7) { # for matrix->screen orientation. todo remove later
  #   for(iy in 1:3 ) {
  #     red[ix, iy] = 1
  #     green[ix, iy] = 0
  #     blue[ix, iy] = 0
  #   }
  # }
  # transpose so horizontal axis is first-dim
  red = t(red)
  green = t(green)
  blue = t(blue)
  for(ix in 1:(cx/2)) {
    tmp = red[ix, ]        # red
    red[ix, ] = red[cx-ix+1, ]
    red[cx-ix+1, ] = tmp
    tmp = green[ix, ]        # green
    green[ix, ] = green[cx-ix+1, ]
    green[cx-ix+1, ] = tmp
    tmp = blue[ix, ]        # blue
    blue[ix, ] = blue[cx-ix+1, ]
    blue[cx-ix+1, ] = tmp
  }
  z = pixmapRGB(c(red, green, blue), cx, cy, bbox=c(0, 0, 1, 1))
  plot(z, axes=TRUE, ...)
  return (invisible(NULL))
}
#' @rdname toPlot
#' @export
toPlot22 = function(spec, ranked=TRUE, ...) {
  toPlot(wfgWrap(2, spec), ranked, inDim=2, ...)
  return (invisible(NULL))
}
