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

