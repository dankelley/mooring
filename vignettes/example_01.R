rm(list=ls())
# name                     radius_m buoyancy_kg   CD  notes
# "Mooring Systems SF-30"  0.381            149   0.5 "buoyancy for 300m"
#
# name                                   radius_m  strength_kg  kgPerMetre CD  notes
# "3/8-inch wire cable"                 0.0047625           NA          NA 1.2 ""
# "Mooring Systems 3X19 3/16\""         0.00325           1814      0.0772 1.2 "https://www.mooringsystems.com/rope.htm"
# ...
# "Hydro Float 20"         0.254            34.5  0.5 ""

u <- 1.00                                # current [m/s]

#float <- "Mooring Systems SF-30"
float <- "Hydro Float 20"
if (float == "Mooring Systems SF-30") {
    # Float "Mooring Systems SF-30"
    dzfloat <- 0.5                         # a guess
    Rfloat <- 0.381                        # radius [m]
    Afloat <- pi * Rfloat^2                # float area
    Bfloat <- 149                          # buoyancy [kg]
} else if (float == "Hydro Float 20") {
    # Float "Hydro Float 20"
    dzfloat <- 0.5                         # a guess
    Rfloat <- 0.254                        # radius [m]
    Afloat <- pi * Rfloat^2                # float area
    Bfloat <- 34.5                         # buoyancy [kg]
} else {
    stop("unknown float")
}

# Cable "Mooring Systems 3x19 3/16"
dzcable <- 1                           # 1-metre chunks of cable
Rcable <- 0.00325                      # radius [m]
Acable <- 2 * dzcable * Rcable         # m^2/m
Bcable <- -0.0772                      # buoyancy [kg], per m
CD <- 1.2
g <- 9.8                               # m/s^2
rho <- 1027                            # kg/m^3 nominal

# Set up area vector for use in drag calculation
A <- c(Afloat, rep(Acable, 100))
# Set up buoyancy 'force', later multiply by g to get a force in Newtons
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


# \phi_2 &= \tan^{-1}\frac{D_1}{B_1}\cr
# T_2 &= \sqrt{D_1^2+B_1^2}
# \phi_i &= \tan^{-1}\frac{D_{i-1}+T_{i-1}\sin\phi_{i-1}}{B_{i-1}+T_{i-1}\cos\phi_{i-1}}\cr
# T_i &= \sqrt{(D_{i-1}+T_{i-1}\sin\phi_{i-1})^2+(B_{i-1}+T_{i-1}\cos\phi_{i-1})^2}

# I don't think L is quite right
L <- abs(diff(z)[1])
X <- rev(L * cumsum(rev(sin(phi))))
Z <- min(z, na.rm=TRUE) + rev(L * cumsum(rev(cos(phi))))

if (!interactive()) pdf("example_01.pdf", width=7, height=3, pointsize=10)
colStagnant <- "gray"
lwdStagnant <- 2
par(mfrow=c(1,2), mar=c(3,3,2,1), mgp=c(2,0.7,0))
ylim <- c(min(z, na.rm=TRUE), 5)
Tstagnant <- cumsum(B)
xlim <- range(c(T/g, Tstagnant/g), na.rm=TRUE)
plot(T/g, Z, xlim=xlim, ylim=ylim, type="l", yaxs="i", xlab="Tension [kg]", ylab="z [m]")
lines(Tstagnant/g, z, col=colStagnant, lwd=lwdStagnant)
grid()

plot(X, Z, ylim=ylim, asp=1, type="l", yaxs="i", xlab="x [m]", ylab="z [m]")
lines(c(0, 0), range(z, na.rm=TRUE), col=colStagnant, lwd=lwdStagnant)
points(0, max(z, na.rm=TRUE), pch=20, col="gray")
points(X[2], Z[2], pch=20)
knockdown <- max(z,na.rm=TRUE) - max(Z,na.rm=TRUE)
mtext(sprintf("%s with u=%.1fm/s: knockdown %.1fm", float, u, knockdown), cex=par("cex"))
grid()
if (!interactive()) dev.off()

# "Mooring Systems SF-30" float with 100m of 3x19-3/16 cable below
# u [m/s]   knockdown [m]
# 0.5                0.38
# 1                  5.49
# 1.5               20.35
# 2                 39.42

# "Hydro Float 20" float with 100m of 3x19-3/16 cable below
# u [m/s]   knockdown [m]
# 0.5                4.10
# 1.0               31.26
# 1.5               57.98
# 2.0               73.78
