library(testthat)
library(mooring)

test_that("element names", {
          l <- 1
          n <- c("model", "buoyancy", "height", "area", "depth")
          A <- anchor(depth=100)
          expect_equal(names(A), c("model", "buoyancy", "height", "area", "depth", "source"))

          CH <- chain(length=l)
          expect_equal(names(CH), c("model", "buoyancy", "height", "area", "CD", "source"))

          CO <- connector()
          expect_equal(names(CO), c("model", "buoyancy", "height", "area", "CD", "source"))

          F <- float()
          expect_equal(names(F), c("model", "buoyancy", "height", "area", "CD", "source"))

          I <- instrument()
          expect_equal(names(I), c("model", "buoyancy", "height", "area", "CD", "source"))

          M <- misc()
          expect_equal(names(M), c("model", "buoyancy", "height", "area", "CD", "source"))

          W <- wire(length=l)
          expect_equal(names(W), c("model", "buoyancy", "height", "area", "CD", "source"))

})


