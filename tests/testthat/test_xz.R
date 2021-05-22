library(mooring)
context("mooring x and y")

test_that("plot with x-y superimposed", {
          expect_silent(m <- anchor(depth=120) + wire(length=100) + float("HMB 20"))
          expect_silent(md <- discretise(m))
          expect_silent(mdk <- knockdown(md, u=1))
          expect_silent(plot(mdk))
          X <- sapply(mdk, function(x) x$x)
          Z <- sapply(mdk, function(x) x$z)
          lines(X,-Z,col=2, type="o", pch=20, cex=0.5)
})
