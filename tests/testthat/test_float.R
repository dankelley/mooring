library(testthat)
library(mooring)

test_that("float creation", {
    # also test that buoyancy etc carry through properly
    f <- float("fake", buoyancy=1, height=2, CD=3, area=1)
    expect_equal(f@model, "fake")
    expect_equal(f@buoyancy, 1)
    expect_equal(buoyancy(f), 1)
    expect_equal(f@height, 2)
    expect_equal(f@CD, 3)
    expect_equal(CD(f), 3)
    expect_equal(f@area, 1)
    expect_equal(area(f), 1)
})
