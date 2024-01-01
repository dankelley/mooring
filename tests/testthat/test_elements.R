library(mooring)

test_that("mooring construction", {
    e <- 1
    expect_error(mooring(e), "need 3 or more arguments")
    expect_error(expect_warning(mooring(e, e, e), "This is not an S7 object"), "first argument must")
    a <- anchor()
    expect_error(mooring(a, e, e), "parameters at the following indices are not")
    expect_error(mooring(anchor(), wire(length = 1), float()), "must specify waterDepth")
    m <- mooring(anchor(), wire(length = 1), float(), waterDepth = 100)
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

#<old feature>test_that("element names", {
#<old feature>    n1 <- c("model", "buoyancy", "height", "area", "CD", "depth", "source")
#<old feature>    n2 <- c("model", "buoyancy", "height", "area", "CD", "source")
#<old feature>    expect_equal(names(anchor()), n1)
#<old feature>    expect_equal(names(chain(length)), n2)
#<old feature>    expect_equal(names(connector()), n2)
#<old feature>    expect_equal(names(float()), n2)
#<old feature>    expect_equal(names(instrument()), n2)
#<old feature>    expect_equal(names(misc()), n2)
#<old feature>    expect_equal(names(wire(length = 1)), n2)
#<old feature>})
