wfg1spec = NA
wfg1 = NA
wfg1t = NA
wfg4spec = NA
wfg4 = NA
wfg4t = NA
wfg5spec = NA
wfg5 = NA
wfg5t = NA
wfg6spec = NA
wfg6 = NA
wfg6t = NA
wfg7spec = NA
wfg7 = NA
wfg7t = NA
wfg9spec = NA
wfg9 = NA
wfg9t = NA

#' wfgSuite
#'
#' Some of the example problems given by the WFG 2006 paper. \cr
#' Implementation note: This is a deferred initialization which brings the definitions into the global environment. \cr
#' They are wfgXspec, wfgX, wfgXt where X is 1, 4, 5, 6, 7, or 9. \cr
#' wfgXSpec is only the declarative specification,  \cr
#' wfgX is the actual evaluating function, \cr
#' wfgXt is its true-front version. \cr
#'
#' @param n 
#'   Number of search-space dimensions
#' @param k 
#'   Number position-related parameters
#' @param M 
#'   Number of objective-space dimensions
#' @return nothing
#' @export
#' @examples
#' # wfgSuite()
#' # wfg1(rep(0.1, 5))

wfgSuite = function(n = 5, k = 3, M = 2) {

  # notes:
  # here are optional "num.pos.rel=k", "0.35...", "0.98...",
  # the "30..." can not be optional. can either be followed by 10 or 95
  assign(envir = .GlobalEnv, "wfg1spec", c(tNone, k, tLinear, NA, 0.35,
              tNone, k, tFlat, NA, 0.8, 0.75, 0.85,
              tPoly, NA, 0.02,
              tSum, M-1, tSum,
              sConvex, M-1, sMixed, overall=1, num=5))
  assign(envir = .GlobalEnv, "wfg1", wfgWrap(M, num.pos.rel=k, wfg1spec) )
  assign(envir = .GlobalEnv, "wfg1t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg1spec) )

  # wfg2,3 see below

  assign(envir = .GlobalEnv, "wfg4spec", c(tMulti, NA, 30, 10, 0.35,
                tSum, M-1, weights=1, tSum, weights=1,
                sConcave) )
  assign(envir = .GlobalEnv, "wfg4", wfgWrap(M, num.pos.rel=k, wfg4spec) )
  assign(envir = .GlobalEnv, "wfg4t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg4spec) )

  assign(envir = .GlobalEnv, "wfg5spec", c(tDecept, NA, 0.35, 0.001, 0.05, 
                tSum, M-1, weights=1, tSum, weights=1,
                sConcave) )
  assign(envir = .GlobalEnv, "wfg5", wfgWrap(M, num.pos.rel=k, wfg5spec) )
  assign(envir = .GlobalEnv, "wfg5t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg5spec) )

  assign(envir = .GlobalEnv, "wfg6spec", c(tNone, k, tLinear,  
                tNonsep, M-1, degree=k/(M-1), tNonsep, degree=n-k,
                sConcave) )
  assign(envir = .GlobalEnv, "wfg6", wfgWrap(M, num.pos.rel=k, wfg6spec) )
  assign(envir = .GlobalEnv, "wfg6t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg6spec) )

  assign(envir = .GlobalEnv, "wfg7spec", list(tParam, k, 0.98/49.98, 0.02, 50, tNone, 
                  tNone, k, tLinear,  
                  tSum, M-1, weights=1, tSum, weights=1,
                  sConcave) )
  assign(envir = .GlobalEnv, "wfg7", wfgWrap(M, num.pos.rel=k, wfg7spec) )
  assign(envir = .GlobalEnv, "wfg7t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg7spec) )

  # wfg8 see below

  assign(envir = .GlobalEnv, "wfg9spec", c(tParam, n-1, 0.98/49.98, 0.02, 50, tNone, 
                tDecept, k, 0.35, 0.001, 0.05, tMulti, NA, 30, 95, 0.35,
                tNonsep, M-1, degree=k/(M-1), tNonsep, degree=n-k,
                sConcave) )
  assign(envir = .GlobalEnv, "wfg9", wfgWrap(M, num.pos.rel=k, wfg9spec) )
  assign(envir = .GlobalEnv, "wfg9t", wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg9spec) )

  # wfg2,3,8 have irregularities in trafo/shape that can not be reproduced by the spec at the moment. (they have to be programmed explicitly calling the trafos/shapes at the moment)

  return (invisible(NULL))
}

