R WFG
=====

This is an implementation of the WFG toolkit. It is a way to construct test functions for multi-criterion optimization problems.

* original paper:
  Huband, Simon, et al. "A review of multiobjective test problems and a scalable test problem toolkit." Evolutionary Computation, IEEE Transactions on 10.5 (2006): 477-506.

This package offers a full implementation of the wfg-toolkit in R. It allows much easier construction of test-functions by providing composition correctness-checks, parameter-sideconstraint-checks and an easy way to specify test-problems (mentioned as a future-work metalanguage in the wfg2006c-paper)

The main functions to use are wfgEval and wfgWrap. wfgEval evaluates a testfunction-specification at a certain search-space point. wfgWrap takes a testfunction-specification and returns a wrapper-function such that this only takes a search-space point as argument and can be given to optimization-algorithms.

A wfg-composed test-function works by first applying transformations, each of them adding a problem-feature typically increasing difficulty and finally setting a shape-function determining the shape of the pareto front.

The testfunction-specification given to wfgEval or wfgWrap is made up of two parts:
- A list-object containing the sequence of transformations and a shape; 
  each transformation and shape can be optionally followed by number of vector-entries to be applied to and any number of the transformation/shape's parameters (in-order or named)
- Additional control arguments, e.g. whether the testfunction should degenerate (see paper).

The code follows a style guide: https://github.com/tudo-r/PackagesInfo/wiki/R-Style-Guide

----

It is recommended that you take a look at wfgTesting.R, it contains examples.

toPlot.R
  contains a visualization showing 2 in- and 2 or 3 out-dimensions
wfgSuite.R
  contains implementations of the original paper's wfg1,4,5,6,7,9 problems using this package
wfgEvalAndWrap.R
  contains wfgEval and wfgWrap with some helper-functions
wfgShape.R
  internal function used to process a shape-spec (using wfgShapes.R)
wfgShapes.R
  the implementations of the various shapes
wfgStudies.R
  some parameter-studies
wfgTesting.R
  unit tests and many small examples
wfgTransformation.R
  internal function used to process a transformation-spec (using wfgTransformations.R)
wfgTransformations.R
  the implementations of the various transformations
wfgUtil.R
  utility functions used by the other files

	Transformations:
tNone    
tPoly
tFlat
tParam
tLinear
tDecept
tMulti
tSum
tNonsep

	Shapes:
sNone       - apply no shape to these vector-entries
sLinear     - linear pareto front
sConcave    - concave pareto front
sConvex     - convex pareto front
sMixed      - pareto front switching between concave/convex
sDisc       - pareto front will have multiple parts


The implementation code parallels the wfg2006c paper wherever possible. It also provides many additions: some features and parameter-checks/explanations.

