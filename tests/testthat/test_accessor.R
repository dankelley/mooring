#<feature removed> library(mooring)
#<feature removed>
#<feature removed> fl <- float()
#<feature removed> m <- mooring(anchor(), wire(length=90), float(), waterDepth = 100)
#<feature removed>
#<feature removed> test_that("[ area", {
#<feature removed>     expect_equal(m["area"], area(m))
#<feature removed> })
#<feature removed>
#<feature removed> test_that("[ buoyancy", {
#<feature removed>     expect_equal(m["buoyancy"], buoyancy(m))
#<feature removed> })
#<feature removed>
#<feature removed> test_that("[ CD", {
#<feature removed>     expect_equal(m["CD"], CD(m))
#<feature removed> })
#<feature removed>
#<feature removed> test_that("[ height", {
#<feature removed>     expect_equal(m["height"], height(m))
#<feature removed> })
#<feature removed>
#<feature removed> test_that("[ on elements", {
#<feature removed>     expect_equal(0.65, fl[["CD"]])
#<feature removed> })
