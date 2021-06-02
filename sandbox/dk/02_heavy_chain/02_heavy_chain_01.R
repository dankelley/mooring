library(mooring)
a <- anchor()
w <- wire("1/2in wire/jack", length=1000)
f <- float()
source("~/git/mooring/R/mooring.R")
m <- mooring(a, w, f)
buoyancy(a)
buoyancy(m)

