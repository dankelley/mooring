## Mooring 1840 (p10 of doc)
library(mooring)
# Abbreviations for convenience
W <- function(length) wire("3/16in galvanized wire coated to 1/4in", length=length)
BUB3 <- float("streamlined bub 3 Viny balls")
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
             connector("ballast", 100/2.2, height=1, width=0.05, CD=1), # guess
             float("syn. float,bracket and 109lb ADCP"), # total guess; lots of choices
             W(149),
             microcat,
             float("streamlined bub 3 Viny balls")) # total guess
md <- discretise(m)
mdk <- knockdown(md, u=function(depth) exp(-depth/1000))
plot(mdk)
