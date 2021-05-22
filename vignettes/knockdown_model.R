## ----echo=FALSE---------------------------------------------------------------
library(mooring)
f <- float("HMB 20")
w <- wire(length=100)

## ----echo=FALSE,dev.args=list(pointsize=9),fig.cap="Response of a mooring consisting of a *Hydro Float 20* atop 100m of *Mooring Systems 3x19 3/16\"* wire cable, in a 1m/s current. **Left**: Cable tension, in kg, with gray for the $u=0$ case and black for $u=1$m/s. **Right:** Mooring shape, with gray and black as in the left panel."----
source("example_01.R")

