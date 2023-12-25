library(mooring)
a <- anchor(depth=1000) # make it deep so a heavy wire can't be held up
par(mfrow=c(1, 2), mar=c(3,3,1,1), mgp=c(2,0.7,0))
wiretypes <- c("1/4in wire/jack", "1/2in wire/jack")
for (wiretype in wiretypes) {
    w <- wire(wiretype, length=900)
    f <- float()
    m <- mooring(a, w, f)
    us <- seq(0, 1.5, 0.05)
    first <- TRUE
    for (dz in c(0.1, 1)) {
        md <- discretise(m, dz)
        zs <- sapply(us, function(u)
                     {
                         mdk <- knockdown(md, u)
                         z(tail(mdk,1))
                     }
        )
        if (first) {
            plot(us, zs, type="o", pch=20, cex=0.5, xlab="Horiz. velo. [m/s]", ylab="Float z [m]")
            first <- FALSE
        } else {
            lines(us, zs, type="o", pch=20, cex=0.5, col=2)
        }
    }
    legend("topright", lwd=1, col=c(1,2), legend=c(0.1, 1), title="dz")
    mtext(paste0("wire type: ", wiretype))
}

