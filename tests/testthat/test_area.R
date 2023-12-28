library(mooring)
test_that("area", {
    # Floats are unaffected by rotation
    expect_equal(area(float()), area(float(), pi / 4))
    # Other elements, e.g. instruments, are affected by rotation
    expect_equal(area(instrument()), area(instrument(), pi / 4.0) * sqrt(2.0))
})
