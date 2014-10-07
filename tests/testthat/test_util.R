context("wfgUtil")

test_that("wfgUtil", {

expect_equal( parseParams(list(aFunc, 1, 2, 3), 1) , list(1, 2, 3) )
expect_equal( parseParams(list(aFunc), 1) , list() )
expect_equal( parseParams(list(aFunc, 4), 1) , list(4) )
expect_equal( parseParams(list(aFunc, 1, NA, 3), 1) , list(1, NA, 3) )
expect_equal( parseParams(list(aFunc, 1, 2, aFunc, 3), 1) , list(1, 2) )
expect_equal( parseParams(list(NA, aFunc, 1, 2, aFunc, 3), 2) , list(1, 2) )
expect_false(isTRUE(all.equal( parseParams(list(aFunc, 1, 2, 2, 3), 1) , list(2, 2, 3) ) ) )
# test named params:
expect_equal( parseParams(list(aFunc, 1, a=2, b=3), 1), list(1, a=2, b=3) )
expect_equal( parseParams(list(aFunc, 1, a=2, b=3, aFunc), 1), list(1, a=2, b=3) )

})

