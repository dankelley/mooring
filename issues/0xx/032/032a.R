library(mooring)
m <- mooring(anchor(depth=10), wire(length=9), float("HMB 20"))
md <- discretise(m, by=1)
mdk <- knockdown(md, u=1)
if (!interactive()) png("032a.png")

par(mfrow=c(2,2))

plot(mdk, which="tension", type="p")
plot(mdk, which="shape", type="p")
plot(mdk, which="knockdown", type="p")

X <- depth(mdk) - depth(mdk, stagnant=TRUE)
Y <- depth(mdk)
points(X, Y, col=2, cex=2)

if (!interactive()) dev.off()

df <- data.frame(depth=depth(mdk),
                 depthOrig=depth(mdk,stagnant=TRUE),
                 drag=drag(mdk,1),
                 tension=tension(mdk))
print(df)
