library(testthat)
library(mooring)

test_that("release creation", {
    # also test that buoyancy etc carry through properly
    r <- release("fake", buoyancy=1, height=2, area=3, CD=4)
    expect_equal(r$model, "fake")
    expect_equal(r$buoyancy, 1)
    expect_equal(buoyancy(r), 1)
    expect_equal(r$height, 2)
    A <- 3
    C_D <- 4
    u <- 1.23
    g <- 9.8
    expect_equal(r$area, A)
    expect_equal(area(r), A)
    expect_equal(r$CD, C_D)
    expect_equal(CD(r), C_D)
    expect_equal(drag(r, u), 0.5 * 1027 * A * C_D * u^2/g)
})

test_that("release default", {
    # just check contents; we know from 'release creation' that the formulae work
    r <- release()
    expect_equal(r$model, "EG&G 723a")
    expect_equal(r$buoyancy, -15.2)
    expect_equal(r$height, 1.194)
    expect_equal(r$area, 0.1173)
    expect_equal(r$CD, 0.65)
    expect_equal(r$source, "BIO")
})
