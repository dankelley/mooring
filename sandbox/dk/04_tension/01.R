library(mooring)
a <- function(depth) anchor("fake", buoyancy = -27, height = 0, depth = depth, CD = 0)
m <- mooring(a(2), wire(length = 1.5), float("16in Viny"))
md <- discretise(m, by = 0.1)
mdk <- knockdown(md, 2.0) # has 22 elements (first is float, last is anchor)
par(mfrow = c(2, 2))
plot(mdk, which = "tension", mar = c(3, 3, 3, 1))

xlim <- par("usr")[1:2]
ylim <- par("usr")[3:4]
pch <- 20
col <- ifelse(is.float(mdk), 4, ifelse(is.anchor(mdk), 2, 1))
cex <- ifelse(is.float(mdk), 2, ifelse(is.anchor(mdk), 2, 1))
plot(tension(mdk), depth(mdk),
    pch = pch, cex = cex, col = col,
    ylim = ylim, xlim = xlim, xaxs = "i", yaxs = "i"
)
mtext("all points (red=anchor, blue=float)")
plot(tension(mdk)[-1], depth(mdk)[-1],
    pch = pch,
    ylim = ylim, xlim = xlim, xaxs = "i", yaxs = "i"
)
mtext("trim float")
look <- 2:(length(mdk) - 1)
plot(tension(mdk)[-c(1, length(mdk))], depth(mdk)[-c(1, length(mdk))],
    pch = pch,
    ylim = ylim, xlim = xlim, xaxs = "i", yaxs = "i"
)
mtext("trim float and anchor")
