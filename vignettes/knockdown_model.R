## ----echo=FALSE---------------------------------------------------------------
library(mooring)
f <- float("HMB 20")
w <- wire(length=100)

## ----echo=FALSE,dev.args=list(pointsize=9),fig.cap="Response of a mooring to a $u=1$m/s current, as described in the text. **Left**: Cable tension, in kg, with gray for the $u=0$ case and black for the $u=1$m/s case. **Right:** Mooring shape, with gray and black as in the left panel."----
source("example_01.R")

