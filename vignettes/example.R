## ----fig.cap="**Diagram for Bedford Institute of Oceanograph mooring 1840 on Sackville Spur**", out.width="70%", fig.align="center", echo=FALSE----
knitr::include_graphics("mooring_1840_enhanced.png")

## -----------------------------------------------------------------------------
# Mooring 1840 (p10 of doc)
library(mooring)

# Abbreviations for convenience
W <- function(length) wire("3/16in galvanized wire coated to 1/4in", length=length)
BUB3 <- float("streamlined BUB 3 Viny balls")
RCM11 <- instrument("RCM-11 in frame") # "AANDERAA RCM11"
microcat <- instrument("SBE37 microcat clamp-on style") # "SBE MICROCAT"

m <- mooring(anchor(depth=1400),
             chain("5/8in galvanized chain", length=10),
             release("benthos 965a"),  # dual benthos 965-a
             BUB3,
             W(34),
             microcat,
             RCM11,
             W(50),
             BUB3,
             W(144),
             microcat,
             RCM11,
             W(147),
             BUB3,
             W(46),
             microcat,
             RCM11,
             W(198),
             microcat,
             RCM11,
             W(198),
             microcat,
             RCM11,
             W(197),
             microcat,
             RCM11,
             W(146),
             microcat,
             connector("swivel"),
             connector("ballast", -100/2.2, height=1, width=0.05, CD=1),
             float("syn. float, bracket and 109lb ADCP"),
             W(149),
             microcat,
             float('new glass streamlined float c2'))
md <- discretise(m)

## ---- fig.cap="**Simulation of knockdown with a depth-decaying current.**", fig.align="center", out.width="100%", dev.args=list(pointsize=9)----
u <- function(depth) 0.5*exp(-depth/1000)
depth <- seq(tail(depth(m), 1), 0, length.out=100)
layout(matrix(1:2,nrow=1), widths=c(0.75,0.25))
mdk <- knockdown(md, u=u)
plot(mdk, "knockdown", xlim=c(0, 40), showDetails=TRUE)
plot(mdk, "velocity")

## -----------------------------------------------------------------------------
md[[155]]

## -----------------------------------------------------------------------------
md[[152]]

## ----fig.cap="**Speed-knockdown summary diagram**", out.width="100%", fig.align="center", dev.args=list(pointsize=9)----
par(mar=c(3,3,1,1), mgp=c(2,0.7,0), mfrow=c(1,1))
iADCP <- 152
imicrocat <- 155
N <- 50
u0 <- seq(0, 0.5, length.out=N)
dz <- rep(NA, N)
U <- rep(NA, N)
for (i in seq_len(N)) {
    u <- function(depth) u0[i] * exp(-depth/1000)
    k <- knockdown(md, u)
    U[i] <- u(-k[[iADCP]]$z)
    dz[i] <- k[[imicrocat]]$z - md[[imicrocat]]$z
}
plot(U, dz, lwd=2, type="l", xlab="Velocity [m/s]", ylab="Knockdown [m]")
grid()
mtext(sprintf("Knockdown of microcat #2, near %.0f m depth", -md[[imicrocat]]$z))

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  curve <- list(U=U, dz=dz)
#  save(curve=curve, file="velocity_knockdown_prediction.rda")

## ----eval=FALSE---------------------------------------------------------------
#  print(m)

## ----eval=FALSE---------------------------------------------------------------
#  m

## ----echo=FALSE---------------------------------------------------------------
m

## -----------------------------------------------------------------------------
m[[2]]

## -----------------------------------------------------------------------------
dput(m[[2]])

## -----------------------------------------------------------------------------
str(m[[2]])

## -----------------------------------------------------------------------------
m[[2]]$width

## -----------------------------------------------------------------------------
m[[2]]$buoyancyPerMeter <- 2 * m[[2]]$buoyancyPerMeter

## -----------------------------------------------------------------------------
W <- function(length) {
    w0 <- wire("3/16in galvanized wire coated to 1/4in", length=1)
    wire("draggy wire", buoyancyPerMeter=w0$buoyancyPerMeter,
         diameter=w0$diameter, CD=2*w0$CD, length=length)
}

## ---- echo=FALSE, fig.cap="**Simulation with doubled wire drag.**", fig.align="center", out.width="100%", dev.args=list(pointsize=9)----
u <- function(depth) 0.5*exp(-depth/1000)
mDraggy <- mooring(anchor(depth=1400),
                   chain("5/8in galvanized chain", length=10),
                   release("benthos 965a"),  # dual benthos 965-a
                   BUB3,
                   W(34),
                   microcat,
                   RCM11,
                   W(50),
                   BUB3,
                   W(144),
                   microcat,
                   RCM11,
                   W(147),
                   BUB3,
                   W(46),
                   microcat,
                   RCM11,
                   W(198),
                   microcat,
                   RCM11,
                   W(198),
                   microcat,
                   RCM11,
                   W(197),
                   microcat,
                   RCM11,
                   W(146),
                   microcat,
                   connector("swivel"),
                   connector("ballast", -100/2.2, height=1, width=0.05, CD=1),
                   float("syn. float, bracket and 109lb ADCP"),
                   W(149),
                   microcat,
                   float('new glass streamlined float c2'))
mdDraggy <- discretise(mDraggy)
layout(matrix(1:2,nrow=1), widths=c(0.75,0.25))
mdkDraggy <- knockdown(mdDraggy, u=u)
plot(mdkDraggy, "knockdown", xlim=c(0, 40), showDetails=TRUE)
plot(mdkDraggy, "velocity")

