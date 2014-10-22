pkgname <- "wfg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('wfg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("nonDominated")
### * nonDominated

flush(stderr()); flush(stdout())

### Name: nonDominated
### Title: nonDominated
### Aliases: nonDominated

### ** Examples

# this shows two values on the two axis and their two search-space coordinates as color and form
value1 = runif(20)
value2 = runif(20)
coord1 = 1:20
coord2 = 20:1
d = cbind(coord1, coord2, value1, value2)
d
nonDom = nonDominated(d, 2)
nonDom
plot(d[,3:4], col=d[, 1], pch=d[, 2], xlim=c(0,1), ylim=c(0,1))
plot(nonDom[,3:4], col=nonDom[, 1], pch=nonDom[, 2], xlim=c(0,1), ylim=c(0,1))



cleanEx()
nameEx("timing")
### * timing

flush(stderr()); flush(stdout())

### Name: timing
### Title: timing
### Aliases: timing

### ** Examples

# usage:
timing(runif(100*1000))

# comparing the evaluation speed of wfgEval and wfgWrap on a 5->2 test function of 4 transformations and 1 shape.
spec4 = c(tPoly, tDecept, tMulti, tNonsep, sConvex)
f4 = wfgWrap(2, spec4 )
timing(f4(rep(0.1, 5)))
timing(wfgEval(rep(0.1, 5), 2, spec4 ) )



cleanEx()
nameEx("toPlot")
### * toPlot

flush(stderr()); flush(stdout())

### Name: toPlot
### Title: toPlot
### Aliases: toPlot toPlot22

### ** Examples

toPlot(wfgWrap(2, c(tDecept, aperture = 0.25, sLinear) ) )

toPlot22( c(tFlat, from=0.4, to=0.9, sLinear) )

toPlot22( c(tMulti, hill.size=0, num.minima=100, sLinear) )

toPlot22( c(tNonsep, degree=10, sLinear) )

# finally an example of showing an optimization algorithm on the objective-landscape:
# (the points returned by the algo will be filtered so only the non-dominated individuals are shown)
# install.packages("mco")
library(mco)
nsga2wrap = function(func, numEvals=1000) {
  # divide numEvals into popsize*generations:
  popsize = floor(sqrt(numEvals)/4)*4
  generations = floor(numEvals/popsize)
  inDim = 2
  return ( nsga2(func, inDim, 2, lower.bounds=rep(0, 5), upper.bounds=rep(1, 5),
                 popsize=popsize, generations=generations) )
}
toPlot22( c(tFlat, from=0.4, to=0.9, sLinear), opt=nsga2wrap)
toPlot22( c(tDecept, aperture = 0.25, sLinear), opt=nsga2wrap)



cleanEx()
nameEx("trueFront")
### * trueFront

flush(stderr()); flush(stdout())

### Name: trueFront
### Title: trueFront
### Aliases: trueFront

### ** Examples

trueFront("truefrontLinear", c(sConvex))



cleanEx()
nameEx("wfgEval")
### * wfgEval

flush(stderr()); flush(stdout())

### Name: wfgEval
### Title: wfgEval and wfgWrap
### Aliases: wfgEval wfgWrap

### ** Examples

v2 = c(0.1, 0.2) # example input vectors
v3 = c(0.1, 0.2, 0.3)
v4 = c(0.1, 0.2, 0.3, 0.4)
v5 = c(0.1, 0.2, 0.3, 0.4, 0.5)

wfgEval(v3, 3, c(sConvex))
       # Specification to only evaluate the convex shape, no transformations.

wfgEval(v3, 3, degen=TRUE, c(sConvex))
       # It is recommended placing any optional parameters in front of the spec (which can be long).

wfgEval(v3, 3, c(sLinear, 2, sMixed))
       # The first parameter is the number of entries to apply the front to. the default is to apply to all remaining entries, except for mixed and disconnected fronts which can only transform a single entry.

wfgEval(v3, 3, c(sDisc, NA, 1.3, sLinear))
       # Order-based parameters can be given, in this case the overall shape is 1.3, the other parameters of the disconnected shape remain their defaults.

wfgEval(v3, 3, c(tFlat, 3, 0.9, 0.2, 0.5, sLinear))
       # A transformation with order-based parameters (number of entries, value, from, to).

wfgEval(v3, 3, c(tDecept, aper=0.25, sLinear))
       # Named parameter (aperture size), all other parameters stay on default.

wfgEval(v3, 3, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, 1, num.minima=30, tNonsep, sConvex, 1, sLinear))
       # Four transformations and two shapes. In this example all transformations are applied to all entries either by using named parameters or in positional parameters by skipping the apply-length with NA.

functor = wfgWrap(2, c(sLinear))
       # Compared to wfgEval the only difference in arguments is, that the first argument - the point z - is not given.



cleanEx()
nameEx("wfgSuite")
### * wfgSuite

flush(stderr()); flush(stdout())

### Name: wfgSuite
### Title: wfgSuite
### Aliases: wfgSuite

### ** Examples

wfgSuite()
wfg1(rep(0.1, 5))



cleanEx()
nameEx("withDominated")
### * withDominated

flush(stderr()); flush(stdout())

### Name: withDominated
### Title: withDominated
### Aliases: baGraphicsWithDominated withDominated

### ** Examples

withDominated(wfgWrap(2, c(sConvex)))
baGraphicsWithDominated("withDominatedTFlat", c(tFlat, from=0.4, to=0.9, sLinear))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
