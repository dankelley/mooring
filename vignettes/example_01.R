library(mooring)

u <- 0.50                                # current [m/s]

f <- float("HMB 20")
dzfloat <- f[[1]]$height
Rfloat <- f[[1]]$height / 2
Afloat <- pi * Rfloat^2
Bfloat <- f[[1]]$buoyancy

w <- wire(length=100)
dzcable <- 1
Rcable <- w[[1]]$width / 2
Acable <- 2 * dzcable * Rcable
Bcable <- w[[1]]$buoyancy
CD <- w[[1]]$CD
g <- 9.8                               # m/s^2
rho <- 1027                            # kg/m^3 nominal


# Set up area vector for use in drag calculation
A <- c(Afloat, rep(Acable, 100))
# Set up buoyancy 'force', later multiply by g to get a force in Newtons
# FIXME: **This only works if the cable sections are 1m long.**
Bkg <- c(Bfloat, rep(Bcable, 100))
# Set up depth (not correct)
n <- length(A)
z <- -c(dzfloat, dzfloat + dzcable * seq(1, 100)) # [m] position of elements

# Drag force [N] ... later, can look up u for z.
D <- 0.5 * A * rho * CD * u^2
# Buoyancy force [N] ... later, could look up (small) changes for stratification
B <- g * Bkg

phi <- rep(NA, n)
T <- rep(NA, n)
phi[2] <- atan2(D[1], B[1]) # radians
T[2] <- sqrt(D[1]^2 + B[1]^2)
for (i in 3:n) {
    phi[i] <- atan2(D[i-1]+T[i-1]*sin(phi[i-1]), B[i-1]+T[i-1]*cos(phi[i-1]))
    T[i] <- sqrt((D[i-1]+T[i-1]*sin(phi[i-1]))^2+(B[i-1]+T[i-1]*cos(phi[i-1]))^2)
}
i <- 1:n

# I don't think L is quite right
L <- abs(diff(z)[1])
X <- rev(L * cumsum(rev(sin(phi))))
Z <- min(z, na.rm=TRUE) + rev(L * cumsum(rev(cos(phi))))

#if (!interactive()) png("example_01.png", width=7, height=3, pointsize=10, unit="in", res=200)
colStagnant <- "gray"
lwdStagnant <- 2
par(mfrow=c(1,2), mar=c(3,3,3,2), mgp=c(2,0.7,0))
ylim <- c(min(z, na.rm=TRUE), 5)
Tstagnant <- cumsum(B)
xlim <- range(c(T/g, Tstagnant/g), na.rm=TRUE)
plot(T/g, Z, xlim=xlim, ylim=ylim, type="l", yaxs="i", xlab="Tension [kg]", ylab="z [m]")
grid()
lines(Tstagnant/g, z, col=colStagnant, lwd=lwdStagnant)
lines(T/g, Z, lwd=2)

plot(X, Z, ylim=ylim, asp=1, type="l", yaxs="i", xlab="x [m]", ylab="z [m]")
grid()
lines(X, Z)
lines(c(0, 0), range(z, na.rm=TRUE), col=colStagnant, lwd=lwdStagnant)
points(0, max(z, na.rm=TRUE), pch=20, col="gray")
points(X[2], Z[2], pch=20)
knockdown <- max(z,na.rm=TRUE) - max(Z,na.rm=TRUE)
mtext(sprintf("%s with u=%.1fm/s", f[[1]]$model, u), cex=par("cex"), line=1)
mtext(sprintf("Knockdown %.1fm ", knockdown), cex=par("cex"), line=0)
#if (!interactive()) dev.off()

# "HFB 20" (hydro float buoy 20") float with 100m of 3x19-3/16 cable below
# u [m/s]   knockdown [m]
# 0.5                7.7
# 1.0               41.2
