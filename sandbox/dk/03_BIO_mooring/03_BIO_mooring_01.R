## Mooring 1840 (p10 of doc)
library(mooring)
#> source("~/git/mooring/R/mooring.R")

# Abbreviations for convenience
W <- function(length) wire("3/16in galvanized wire coated to 1/4in", length=length)
BUB3 <- float("streamlined BUB 3 Viny balls")
RCM11 <- instrument("RCM-11 in frame") # "AANDERAA RCM11"
microcat <- instrument("SBE37 microcat clamp-on style") # "SBE MICROCAT"

m <- mooring(anchor(depth=1400),
             chain("5/8in galvanized chain", length=10),
             release("benthos 965a release"),  # dual benthos 965-a
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
             connector("ballast", -100/2.2, height=1, width=0.05, CD=1), # guess
             float("syn. float, bracket and 109lb ADCP"), # total guess; lots of choices
             W(149),
             microcat,
             # For now, I would use the numbers in 'NEW GLASS STREAMINED FLOAT (C2)', with 100 lbf [445 N]. This is the Open Seas C2 SUBS float. The 'STREAMLINED BUB 2 x 17" GLASS' might be from a handful of prototypes we had made, tested and used.
             float('new glass streamlined float c2'))
md <- discretise(m)
print(md)

u <- function(depth) 0.5*exp(-depth/1000)
depth <- seq(m[[1]]$depth, 0, length.out=100)
if (!interactive())
    png("mooring_01.png", width=7, height=5, unit="in", res=200, pointsize=9)
layout(matrix(1:2,nrow=1),widths=c(0.7,0.3))
mdk <- knockdown(md, u=u)
#plot(mdk, showDetails=TRUE)
plot(mdk, "knockdown", showDetails=TRUE, xlim=c(0, 15))
ylim <- par('usr')[3:4]
plot(mdk, "velocity")
if (!interactive())
    dev.off()

