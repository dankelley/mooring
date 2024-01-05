library(testthat)
library(mooring)

test_that("mooring construction", {
    m <- mooring(anchor(), wire(length=100), float(), waterDepth = 200)
    b <- buoyancy(m) # kg
    expect_equal(b, c(320, -13, -350))
    expect_equal(rep(0, 3), x(m))
    expect_equal(c(-98.85, -99.85, -199.85), z(m))
    expect_equal(c(98.85, 99.85, 199.85), depth(m))
})

test_that("plot with x-y superimposed", {
    m <- mooring(anchor(), wire(length=100), float("HMB 20"), waterDepth = 120)
    expect_silent(md <- segmentize(m))
    expect_silent(mdk <- knockdown(md, u=1))
    expect_silent(draw(mdk))
    X <- sapply(mdk@elements, \(e) e@x)
    Z <- sapply(mdk@elements, \(e) e@z)
    lines(X, -Z, col=2, type="o", pch=20, cex=0.5)
})
