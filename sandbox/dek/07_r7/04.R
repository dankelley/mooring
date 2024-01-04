source("03.R")
source("anchor_test.R")
source("float_test.R")
source("wire_test.R")
testthat::expect_error(mooring(a = 1, b = 2), "element 1 is not an anchor")
float_test()
float_test("?")
f <- float_test(model = "testFloat", buoyancy = 3, height = 0.2, area = 0.04, CD = 1.3)
str(f)
# dput(f)
f@buoyancy
# buoyancy(f)
class(f)
anchor_test(depth = 100)
# mooring::drag
# mooring::mooring

a <- anchor_test(depth = 999)
w <- wire_test(length = 100)
f <- float_test()
m <- mooring(a, w, f)
m

message("FIXME to do list:")
message(" * add wire()")
message(" * add connector()")
message(" * add misc()")
message(" * add some other things, I bet")
