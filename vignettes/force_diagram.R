library(mooring)
source("~/git/mooring/R/mooring.R")
m <- anchor() + wire(height=1) + float()
mk <- knockdown(m, 1)
str(mk)
plot(mk)
mk


cex <- 1
pch <- 20
col <- "black"
plot(0:1, 0:1, xlab="", ylab="")
x0 <- y0 <- 0.5
phi_i <- 20
phi_i_plus_1 <- 30
T_i <- 1
T_i_plus_1 <- 1
points(x0, y0, pch=pch, cex=cex, col=col)

grid()
