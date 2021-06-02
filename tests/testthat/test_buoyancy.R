library(testthat)
library(mooring)

test_that("specified anchor buoyancy is correct", {
          expect_equal(-1000, buoyancy(anchor("trainwheel", depth=120)))
})

test_that("default anchor buoyancy is correct", {
          expect_equal(-1000, buoyancy(anchor(depth=120)))
})

test_that("mooring is correct and unchanged", {
          m <- mooring(anchor(depth=200), wire(length=100), float())
          b <- buoyancy(m)
          expect_equal(b, c(-1000, -13, 320))
          expect_equal(rep(0, 3), x(m))
          expect_equal(c(-200, -100, -99), z(m))
          expect_equal(c(200, 100, 99), depth(m))
})

