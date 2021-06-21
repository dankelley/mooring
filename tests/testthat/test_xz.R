library(testthat)
library(mooring)

test_that("mooring construction", {
          m <- mooring(anchor(depth=200), wire(length=100), float())
          b <- buoyancy(m)
          expect_equal(b, c(320, -13, -1000))
          expect_equal(rep(0, 3), x(m))
          expect_equal(c(-99, -100, -200), z(m))
          expect_equal(c(99, 100, 200), depth(m))
})

test_that("plot with x-y superimposed", {
          expect_silent(m <- mooring(anchor(depth=120), wire(length=100), float("HMB 20")))
          expect_silent(md <- discretise(m))
          expect_silent(mdk <- knockdown(md, u=1))
          expect_silent(plot(mdk))
          X <- sapply(mdk, function(x) x$x)
          Z <- sapply(mdk, function(x) x$z)
          lines(X,-Z,col=2, type="o", pch=20, cex=0.5)
})
