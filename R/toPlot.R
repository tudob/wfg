# TODO: 
# find out whether this kind of plot is considered good/bad. name?

# note pixmapRGB issues a warning: 'x' is NULL so the result will be NULL
#     however, pixmapRGB does not have a parameter 'x', so it has to be some problem internal to pixmapRGB

# generates rgb-bitmaps then uses package pixmap to show the bitmaps
# install.packages("pixmap")
	# library(pixmap)
# ?pixmapRGB

#' toPlot
#'
#' A visualization used alongside the WFG package. It shows for each point in the discretized search-space which objective-values will be realized by it (as color).\cr
#' At the moment the first two search-space dimensions are selected (this should be parameterized in the future).\cr
#' It is possible to show an RGB- or Red-Blue-plot (chosen because of common red-green blindness).\cr
#' Red-Blue: Red shows a large value in the first objective, Blue shows a large value in the second objective.\cr
#' Green dots show the non-dominated individuals of an optimization algorithm (which can internally run multiple times), it is surprisingly instructive.\cr
#' toPlot22 is a wrapper taking a wfg spec instead of an arbitrary function.\cr
#'
#' @param func 
#'   The function to be shown
#' @param ranked 
#'   Decides whether the colors are chosen based on ranked objective-values. This is recommended to distinguish colors but it does not allow interpretation of their absolute values.
#' @param inDim 
#'   Number of search-space dimensions; needed for the optimization algorithm
#' @param opt.algo 
#'   An optimization algorithm may be specified. Its non-dominated individuals are shown.
#' @param ...
#'    Parameters to be passed through to the plot function
#' @param spec
#'    In plot22 - the spec for the test function to plot
#' @return nothing
#' @export
#' @import BBmisc
#' @examples
#'
#' toPlot(wfgWrap(2, c(tDecept, aperture = 0.25, sLinear) ) )
#'
#' toPlot22( c(tFlat, from=0.4, to=0.9, sLinear) )
#'
#' toPlot22( c(tMulti, hill.size=0, num.minima=100, sLinear) )
#'
#' toPlot22( c(tNonsep, degree=10, sLinear) )
#'
#' # finally an example of showing an optimization algorithm on the objective-landscape:
#' # (the points returned by the algo will be filtered so only the non-dominated individuals are shown)
#' # install.packages("mco")
#' library(mco)
#' nsga2wrap = function(func, numEvals=1000) {
#'   # divide numEvals into popsize*generations:
#'   popsize = floor(sqrt(numEvals)/4)*4
#'   generations = floor(numEvals/popsize)
#'   inDim = 2
#'   return ( nsga2(func, inDim, 2, lower.bounds=rep(0, 5), upper.bounds=rep(1, 5), 
#'                  popsize=popsize, generations=generations) )
#' }
#' toPlot22( c(tFlat, from=0.4, to=0.9, sLinear), opt=nsga2wrap)
#' toPlot22( c(tDecept, aperture = 0.25, sLinear), opt=nsga2wrap)

toPlot = function(func, ranked=TRUE, inDim=5, opt.algo=NA, ...) {
  requirePackages("pixmap", why = "toPlot")

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
  # res = randomsearch(func, 1000, inDim)
  if(is.function(opt.algo)) {
    res = opt.algo(func);

    plot(res$value, ...) # plot the p-Front and dominated(!) points
    res = cbind(res$par, res$value)

    str(res)
    cat("WITH ", inDim, "\n")
    nonDom = nonDominated(res, inDim)
    values = nonDom[, (inDim+1):ncol(nonDom)]
    plot(values, ...) # plot the p-Front
    
    px = nonDom[, 1]
    px[px==1.0] = 0.9999 # fix. 1 would possibly result in invalid indexing below
    px = floor(px*cx) # real-coord -> pixel     # todo change if coords are not 0-1 anymore
    
    py = nonDom[, 2]
    py[py==1.0] = 0.9999 # fix. 1 would possibly result in invalid indexing below
    py = floor(py*cy)
    
    # draw opt.algo results into matrix
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
  } else {
    if(!is.na(opt.algo)) stop("non-function object given as opt.algo") # prevent accidents
  }

  # plot the param-space. the opt.algo points were added into the matrix
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
toPlot22 = function(spec, ranked=TRUE, opt.algo=NA, ...) {
  toPlot(wfgWrap(2, spec), ranked, inDim=2, opt.algo, ...)
  return (invisible(NULL))
}
