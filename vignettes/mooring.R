## -----------------------------------------------------------------------------
library(mooring)
m <- mooring(anchor(depth=200), wire(length=150), float())

## -----------------------------------------------------------------------------
wire("?")

## -----------------------------------------------------------------------------
md <- discretise(m) # breaks wire portions into approx. 1-m segments

## -----------------------------------------------------------------------------
mdk <- knockdown(md, u=1)

## ----fig.width=7, fig.height=3, dev.args=list(pointsize=9)--------------------
plot(x(mdk), z(mdk), type="l")

## ----fig.width=7, fig.height=3, dev.args=list(pointsize=9)--------------------
par(mfrow=c(1, 2))
plot(mdk, which="tension")
plot(mdk, which="shape")

## ----fig.width=7, fig.height=3, dev.args=list(pointsize=9)--------------------
par(mfrow=c(1, 2))
plot(mdk, which="tension", fancy=TRUE)
plot(mdk, which="shape", fancy=TRUE)

## -----------------------------------------------------------------------------
i <- instrument("myNewInstrument", buoyancy=-5, height=1, area=0.1, CD=1)

## -----------------------------------------------------------------------------
w <- wire(length=100) # default wire type
w2 <- w
w2$CD <- 2 * w2$CD

## -----------------------------------------------------------------------------
c(drag(w, 1), drag(w2, 1))

