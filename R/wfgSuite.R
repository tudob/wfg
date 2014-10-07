# these are some definitions for some of the wfg1-9 sample-problems

n = 5
k = 3
M = 2

# here are optional "num.pos.rel=k", "0.35...", "0.98...",
# the "30..." can not be optional. can either be followed by 10 or 95

wfg1spec = c(tNone, k, tLinear, NA, 0.35,
            tNone, k, tFlat, NA, 0.8, 0.75, 0.85,
            tPoly, NA, 0.02,
            tSum, M-1, tSum,
            sConvex, M-1, sMixed, overall=1, num=5)
wfg1 = wfgWrap(M, num.pos.rel=k, wfg1spec)
wfg1t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg1spec)

# wfg2,3 see below

wfg4spec = c(tMulti, NA, 30, 10, 0.35,
              tSum, M-1, weights=1, tSum, weights=1,
              sConcave)
wfg4 = wfgWrap(M, num.pos.rel=k, wfg4spec)
wfg4t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg4spec)

wfg5spec = c(tDecept, NA, 0.35, 0.001, 0.05, 
              tSum, M-1, weights=1, tSum, weights=1,
              sConcave)
wfg5 = wfgWrap(M, num.pos.rel=k, wfg5spec)
wfg5t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg5spec)

wfg6spec = c(tNone, k, tLinear,  
              tNonsep, M-1, degree=k/(M-1), tNonsep, degree=n-k,
              sConcave)
wfg6 = wfgWrap(M, num.pos.rel=k, wfg6spec)
wfg6t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg6spec)

wfg7spec = list(tParam, k, 0.98/49.98, 0.02, 50, tNone, 
                tNone, k, tLinear,  
                tSum, M-1, weights=1, tSum, weights=1,
                sConcave)
wfg7 = wfgWrap(M, num.pos.rel=k, wfg7spec) 
wfg7t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg7spec) 

# wfg8 see below

wfg9spec = c(tParam, n-1, 0.98/49.98, 0.02, 50, tNone, 
              tDecept, k, 0.35, 0.001, 0.05, tMulti, NA, 30, 95, 0.35,
              tNonsep, M-1, degree=k/(M-1), tNonsep, degree=n-k,
              sConcave)
wfg9 = wfgWrap(M, num.pos.rel=k, wfg9spec) 
wfg9t = wfgWrap(M, num.pos.rel=k, true.front=TRUE, wfg9spec) 

# wfg2,3,8 have irregularities in trafo/shape that can not be reproduced by the spec at the moment. (they have to be programmed explicitly calling the trafos/shapes at the moment)

