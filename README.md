R WFG
=====

This is an implementation of the WFG toolkit. It is a way to construct test functions for multi-criterion optimization problems.

* original paper:
  Huband, Simon, et al. "A review of multiobjective test problems and a scalable test problem toolkit." Evolutionary Computation, IEEE Transactions on 10.5 (2006): 477-506.

This package offers a full implementation of the wfg-toolkit in R. It allows much easier construction of test-functions by providing composition correctness-checks, parameter-sideconstraint-checks and an easy way to specify test-problems (mentioned as a future-work in the wfg2006c-paper)

The main functions to use are wfgEval and wfgWrap. wfgEval evaluates a testfunction-specification at a certain search-space point. wfgWrap takes a testfunction-specification and returns a wrapper-function such that this only takes a search-space point as argument and can be given to optimization-algorithms.

A wfg-composed test-function works by first applying transformations, each of them adding a problem-feature typically increasing difficulty and finally setting a shape-function determining the shape of the pareto front.

The testfunction-specification given to wfgEval or wfgWrap is made up of two parts:
- A list-object containing the sequence of transformations and a shape; 
  each transformation and shape can be optionally followed by number of vector-entries to be applied to and any number of the transformation/shape's parameters (in-order or named)
- Additional control arguments, e.g. whether the testfunction should degenerate (see paper).

To start using it right away refer to the man pages:
?wfgEval
?wfgTrafos
?wfgShapes
  Also of interest may be:
The visualizations: ?trueFront, ?withDominated, ?toPlot
As well as the utility functions: ?nonDominated, ?randomSearch, ?timing
And the example-problems of the wfg-paper: ?wfgSuite

Many more examples can be found in tests/testthat/test_wfgEvalAndWrap.R.

The code follows a style guide: https://github.com/tudo-r/PackagesInfo/wiki/R-Style-Guide

The implementation code parallels the wfg2006c paper wherever possible. It also provides additions: some features and many parameter-checks and explanations.

