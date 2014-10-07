# This is a combination of functional tests and tutorial/examples

# tPoly 
#   alpha>0    (default 0.02)
# tDecept
#   optimumLocation    in (0, 1)  (default 0.35)
#   apertureAroundOptimum 0<B<<1      (default 0.001)
#   deceptiveValue    0<C<<1    (default 0.05)
#   (additional constraints)
# tMulti
#   numMinima  natural number    (default 30)
#   hillSize    >0        (default 95.0)
#   optimumLocation  in (0, 1)    (default 0.35)
#   (additional constraints)
# tNonsep
#   degree    natural number  1...|y|  with:  |y| mod degree = 0    

context("wfgEvalAndWrap")

v2 = c(0.1, 0.2) # example input vectors
v3 = c(0.1, 0.2, 0.3)
v4 = c(0.1, 0.2, 0.3, 0.4)
v5 = c(0.1, 0.2, 0.3, 0.4, 0.5)

values = function(vec) {
	return ( !is.null(vec) && !any(is.na(vec)) )
}

test_that("wfgEvalAndWrap", {

# only a shape, no transformations:

expect_true( values( wfgEval(v2, 2, c(sLinear)) ) )
expect_true( values( wfgEval(v2, 2, noise=0.01, c(sLinear)) ) ) # (user should give named args before the spec because the spec can get long)

expect_true( values( wfgEval(v3, 2, c(sLinear)) ) )
expect_true( values( wfgEval(v4, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v4, 4, c(sLinear)) ) )

expect_true( values( wfgEval(v5, 2, c(sLinear)) ) )
expect_true( values( wfgEval(v5, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v4, 3, c(sLinear)) ) )

expect_error(wfgEval( v2, 1, c(sLinear)) )

expect_error(wfgEval( v2, 2, c(sLinear, sLinear)) )
expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(sLinear, 1, sLinear)) ) )

expect_true( values( wfgEval(v3, 2, c(sLinear, 2)) ) )
expect_error(wfgEval( v3, 2, c(sLinear, 1.1)) )

expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_error(wfgEval( v3, 3, c(sLinear, 2)) )
expect_true( values( wfgEval(v3, 3, c(sLinear, 2, sLinear)) ) )

expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sLinear)) ) )
expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sConvex)) ) )
expect_true( values( wfgEval(v2, 2, c(sConvex, 1, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(sLinear, 1, sLinear)) ) )

expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sMixed)) ) )

expect_error(wfgEval( v2, 3, c(sLinear, 1, sMixed)) )
expect_true( values( wfgEval(v3, 3, c(sLinear, 2, sMixed)) ) )
expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sMixed)) ) )
expect_true( values( wfgEval(v2, 2, c(sLinear, 1, sDisc)) ) )
expect_true( values( wfgEval(v2, 2, c(sConvex, 1, sMixed)) ) )
expect_true( values( wfgEval(v2, 2, c(sConvex, 1, sMixed)) ) )
expect_true( values( wfgEval(v2, 2, c(sMixed, sDisc)) ) )

expect_true( values( wfgEval(v2, 2, c(sLinear)) ) )
expect_error(wfgEval( v2, 2, degen=TRUE, c(sLinear)) )
expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 3, degen=TRUE, c(sLinear)) ) )

for(i in 1:1000) expect_true( values( wfgEval(v2, 2, c(sLinear)) ) )

expect_true( values( wfgEval(v2, 2, c(sLinear, 2)) ) )
expect_error(wfgEval( v2, 2, c(sLinear, 3)) )
expect_error(wfgEval( v2, 5, c(sLinear)) )
expect_error(wfgEval( v2, 2, c(sLinear, 1)) )
expect_error(wfgEval( v2, 2, c(sLinear, 7)) )
expect_true( values( wfgEval(v2, 2, c(sConvex)) ) )
expect_true( values( wfgEval(v2, 2, c(sConcave)) ) )
expect_error(wfgEval( v2, 1, c(sMixed)) )
expect_error(wfgEval( v2, 2, c(sMixed)) )
expect_error(wfgEval( v2, 2, c(sDisc)) )
expect_true( values( wfgEval(v2, 2, c(sMixed, sMixed)) ) )

# in-order params:

expect_true( values( wfgEval(v2, 2, c(sMixed, NA, 0.7, sMixed)) ) ) # order-based params: NA skips applylength, 0.7 is 'overall'
expect_error(wfgEval( v3, 3, c(sMixed, NA, 0.7, sMixed)) )
expect_true( values( wfgEval(v3, 3, c(sMixed, NA, 0.7, 4, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(sDisc, NA, 0.7, 4, 0.5, sLinear)) ) )

# named params:

expect_true( values( wfgEval(v2, 2, c(sMixed, overall=0.7, num=3, sMixed)) ) ) # ok these two should be equal
expect_true( values( wfgEval(v2, 2, c(sMixed, num=3, overall=0.7, sMixed)) ) )

# order of trafos/shapes is critical, ie these are different:
expect_true( values( wfgEval(v4, 4, c(sMixed, overall=0.6, num=3, sMixed, overall=0.4, num=3, sLinear)) ) )
expect_true( values( wfgEval(v4, 4, c(sMixed, overall=0.4, num=3, sMixed, overall=0.6, num=3, sLinear)) ) )

expect_true( values( wfgEval(v2, 2, c(sMixed, overall=0.7, sMixed, overall=0.8)) ) ) # duplicate param-names are ok

expect_true( values( wfgEval(v4, 3, c(sMixed, num=3, overall=0.7, sLinear)) ) )
expect_true( values( wfgEval(v4, 3, degen=TRUE, c(sMixed, num=3, overall=0.7, sLinear)) ) )

# with transformations:

expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tPoly, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tPoly, alpha=0.3, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tPoly, alpha=1.4, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tPoly, alpha=1.4, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tPoly, 3, alpha=1.4, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tPoly, 3, 1.4, sLinear)) ) )
expect_error(wfgEval( v3, 3, c(tPoly, 1, alpha=1.4, sLinear)) )
expect_true( values( wfgEval(v3, 3, c(tPoly, alpha=1.4, tPoly, alpha=1.1, sLinear)) ) )
expect_true( values( wfgEval(v4, 3, c(tPoly, 1, alpha=1.4, tFlat, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tFlat, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tFlat, 3, 0.9, 0.2, 0.5, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tFlat, 3, 0.1, 0.001, 0.999, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tParam, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tParam, factor=0.3, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tParam, NA, 0.05, 0.1, 10, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tLinear, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tLinear, zero.loc=0.7, sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tDecept, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tDecept, aper=0.25, sLinear)) ) ) # much easier to optimize than the default
expect_true( values( wfgEval(v3, 3, c(tDecept, dec=0.2, sLinear)) ) ) # test whether for a given algo the *value* of the deceptive makes a difference
wfgEval(v3, 3, c(tMulti, sLinear))
expect_true( values( wfgEval(v3, 3, c(tMulti, NA, 5, 5, sLinear)) ) ) # much easier to optimize than the default

expect_true( values( wfgEval(v3, 3, c(tSum, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, list(tSum, NA, 1, 3, rep(1, 3), sLinear)) ) ) # with weights-vector. note: need list() to prevent the weights from being flattened
expect_error(wfgEval( v3, 3, list(tSum, NA, 1, 3, rep(1, 2), sLinear)) )

expect_true( values( wfgEval(v3, 2, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 2, c(tNonsep, sLinear)) ) ) # no difference?
expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tNonsep, sLinear)) ) ) # difference. the reason for no difference above is sLinear on 2-dim (the linear p-front is a line of constant value 1 in the first dim, and going from 0 to 2 in the second dim)

expect_true( values( wfgEval(v3, 3, c(sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tSum, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tSum, 1, tNone, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, c(tSum, 1, tNone, sLinear)) ) )
expect_true( values( wfgEval(v3, 3, list(tSum, from=1, to=3, weights=c(1.5, 1, 0.5), sLinear)) ) )

expect_true( values( wfgEval(v3, 3, c(tParam, sLinear)) ) )

# like in the param-study:

expect_true( values( wfgEval(v3, 3, c(tPoly, tDecept, tMulti, tNonsep, sLinear)) ) ) # only default values

# in the following only tNonsep's degree is left to the package.
expect_true( values( wfgEval(v3, 3, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35, tNonsep, sLinear)) ) )

expect_true( values( wfgEval(v3, 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) ) )

expect_true( values( wfgEval(v5, 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) ) )
expect_true( values( wfgEval(runif(5), 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) ) )

expect_true( values( wfgEval(runif(5), 2, c(sLinear)) ) ) # always 1.0 as first
expect_true( values( wfgEval(runif(2), 2, c(sLinear)) ) ) # always 1.0 as first

expect_true( values( wfgEval(runif(5), 2, c(sConvex)) ) ) # different stuff in first


# get a wrapper function which can be directly passed to algorithms:
f2 = wfgWrap(2, c(sLinear))
expect_true( values( f2(v2) ) )
for(i in 1:1000) f2(v2)
f3 = wfgWrap(3, c(sLinear, 2, sMixed))
expect_true( values( f3(v3) ) )

})
