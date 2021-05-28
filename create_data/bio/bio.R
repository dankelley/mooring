library(mooring)
library(testthat)
rm(list=ls())
lines <- readLines("moorspec-4.csv")
lines2 <- gsub("  ", " ", lines)
lines3 <- gsub("\\*\\*", "", lines2)
# change some odd special-case things
lines4 <- gsub("\\(-1500\\)", "-1500", lines3)
lines5 <- gsub("nt/m", "", lines4)
lines6 <- gsub("\\(15\\)*", "15", lines5)
lines7 <- gsub("15\\*", "15", lines6)

d <- read.csv(text=lines7)
names(d)

# wires: two entries, in top two lines. We check widths against names.
isWire <- grep("WIRE", d$description)
wire <- d[isWire, ]
wireWidth <- wire$area1 # metres
stopifnot(all.equal(16*wireWidth/0.0254, c(4,5)))

d <- d[-isWire,]
head(d, 1)


# chop out some things, just to get it so I can read the key columns on my screen
d <- d[, -grep("number", names(d))]    # first column
d <- d[, -grep("^X", names(d))]        # empty columns at end
d <- d[, -grep("length1", names(d))]   # not sure what the two length columns are
d <- d[, -grep("LBS", names(d))]       # we have Newtons beside it
d <- d[, -grep("DRAG_N", names(d))]    # could get CD from this if we knew the experiment
d <- d[, -grep("DRAG_T", names(d))]    # could get CD from this if we knew the experiment
# We will ignore some other columns, just by not using them.
print(names(d))
head(d,3)
type <- tolower(d$description)
height <- as.numeric(d$length2)
area <- as.numeric(d$area)
newtons <- as.numeric(d$NEWTONS)
buoyancy <- newtons / 9.8
width <- 2*sqrt(area)/pi               # diameter, if it's a wire
dd <- data.frame(type=type, buoyancy=buoyancy, height=height, area=area, width=width)

print(dd[1,])
w <- wire()                            # Dewey "1/4 wire/jack"
rDewey <- 0.5 * 1/4 * 0.0254
aDewey <- pi * rDewey^2
widthDewey <- w$width
print(aDewey)
print(dd[1,]$area)
dd[1,]$area / aDewey # match to 1%
message("how to know if it's a wire (meaning we need to compute width)?")

