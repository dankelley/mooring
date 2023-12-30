library(testthat)
library(mooring)

test_that("specified anchor buoyancy is correct", {
    expect_equal(-1050, buoyancy(anchor("3 Railway Wheels", depth=120)))
})

test_that("default anchor buoyancy is correct", {
    expect_equal(-350, buoyancy(anchor(depth=120)))
})

test_that("wire buoyancy proportional to length", {
    expect_equal(3*buoyancy(wire(length=1)), buoyancy(wire(length=3)))
})

test_that("chain buoyancy proportional to length", {
    expect_equal(3*buoyancy(chain(length=1)), buoyancy(chain(length=3)))
})
