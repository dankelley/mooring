library(testthat)
library(mooring)

test_that("specified anchor buoyancy is correct", {
          expect_equal(-1000, buoyancy(anchor("trainwheel", depth=120)))
})

test_that("default anchor buoyancy is correct", {
          expect_equal(-1000, buoyancy(anchor(depth=120)))
})

