library(mooring)

test_that("mooring construction", {
    e <- 1
    expect_error(mooring(e), "need 3 or more arguments")
    expect_error(mooring(e, e, e), "first argument must be created with anchor()")
    a <- anchor()
    expect_error(mooring(a, e, e), "parameters at the following indices are not")
    m <- mooring(anchor(), wire(length = 1), float())
    expect_true(is.mooring(m))
    expect_true(is.anchor(anchor()))
})

test_that("element is.*()", {
    expect_true(is.anchor(anchor()))
    expect_true(is.float(float()))
    expect_false(is.float(anchor())) # don't bother testing every combination
    expect_true(is.instrument(instrument()))
    expect_true(is.wire(wire(length = 1)))
    expect_true(is.chain(chain(length = 1)))
    expect_true(is.connector(connector()))
    expect_true(is.instrument(instrument()))
})

test_that("element names", {
    n1 <- c("model", "buoyancy", "height", "area", "CD", "depth", "source")
    n2 <- c("model", "buoyancy", "height", "area", "CD", "source")
    expect_equal(names(anchor(depth = 1)), n1)
    expect_equal(names(chain(length = 1)), n2)
    expect_equal(names(connector()), n2)
    expect_equal(names(float()), n2)
    expect_equal(names(instrument()), n2)
    expect_equal(names(misc()), n2)
    expect_equal(names(wire(length = 1)), n2)
})
