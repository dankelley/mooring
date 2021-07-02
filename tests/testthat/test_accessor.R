library(testthat)
library(mooring)

F <- float()
m <- mooring(anchor(depth=100), wire(length=90), float())

test_that("[ area", {
          expect_equal(m["area"], area(m))
})

test_that("[ buoyancy", {
          expect_equal(m["buoyancy"], buoyancy(m))
})

test_that("[ CD", {
          expect_equal(m["CD"], CD(m))
})

test_that("[ height", {
          expect_equal(m["height"], height(m))
})

test_that("[ on elements", {
          expect_equal(0.65, F["CD"])
})

