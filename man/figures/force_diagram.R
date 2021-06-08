library(testthat)
L <- 0.5                               # tank depth, m
diameter <- 0.04 # ping-pong balls have 40mm diameter
R <- diameter / 2
area <- pi*R^2
volume <- 4/3*pi*R^3
rho <- 1027
deltaRho <- rho                        # water vs air (round numbers)
g <- 9.8
Bkg <- deltaRho * volume # kg
B <- g * Bkg
CD <- 1
u <- 0.9                               # speed, m/s
D <- 0.5 * CD * area * rho * u^2
phi <- atan2(D, B)
x <- L * sin(phi)
l <- L * cos(phi)
T <- B / cos(phi)

library(mooring)
f <- float("pingpong", Bkg, height=0, diameter=2*R, CD=1)
expect_equal(Bkg, buoyancy(f))
expect_equal(D, drag(f, u=u))

w <- wire("gossamer", length=L, buoyancyPerMeter=0, CD=0, diameter=0)
a <- anchor("fake", buoyancy=-1000, height=0, depth=L)
m <- mooring(a, w, f)

n <- 500 # number of sub-segments to use
md <- discretise(m, L/n)
mdk <- knockdown(md, u=u)

if (!interactive())
    png("force_diagram.png", width=5, height=3, unit="in", res=120, pointsize=10)
plot(mdk, fancy=TRUE, showDepths=FALSE, showLabels=FALSE)

scale <- 0.2
length <- 0.1
col <- 2
lwd <- 3
cex <- 1.3*par("cex")
arrows(x, L-l, x, L-l-scale*f$buoyancy*9.8, length=length, col=col, lwd=lwd)
text(x, L-l-scale*f$buoyancy*9.8, "B", font=2, col=col, cex=cex, pos=3)

# Drag
arrows(x, L-l, x+scale*D, L-l, length=length, col=col, lwd=lwd)
text(x+scale*D, L-l, "D", font=2, col=col, cex=cex, pos=4)

Tx <- -T * sin(phi)
Tz <- T * cos(phi)
arrows(x, L-l, x+scale*Tx, L-l+scale*Tz, length=length, col=col, lwd=lwd)
text(x+scale*Tx, L-l+scale*Tz, "T", font=2, col=col, pos=1, cex=cex)
if (!interactive())
    dev.off()

